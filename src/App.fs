module BasicCanvas

open System
open Fable.Core.JsInterop

type [<Measure>] radian
type [<Measure>] degree

type Point = Point of x: float * y: float
type Circle = Circle of point: Point * radius: float
type IntersectionTree = Element of Circle * IntersectionTree list

module List =
    let allPairs xs ys = [
        for x in xs do
        for y in ys do
        yield x, y
    ]

let radian (degree: float<degree>): float<radian> =
    degree * Math.PI / 180. * 1.<radian/degree>

let angle (Point (x1, y1)) (Point (x2, y2)): float<degree> =
    Math.Atan2(y2 - y1, x2 - x1) * (180.<degree> / Math.PI)

let intersection (Circle (Point (x0, y0), r0)) (Circle (Point (x1, y1), r1)) =
    let dx = x1 - x0
    let dy = y1 - y0
    let d = Math.Sqrt((dy*dy) + (dx*dx))
    if (d > (r0 + r1)) then
        None
    else if (d < Math.Abs(r0 - r1)) then
        None
    else
        let a = ((r0*r0) - (r1*r1) + (d*d)) / (2.0 * d)
        let x2 = x0 + (dx * a/d)
        let y2 = y0 + (dy * a/d)
        let h = Math.Sqrt((r0*r0) - (a*a))
        let rx = -dy * (h/d)
        let ry = dx * (h/d)
        let xi = x2 + rx
        let xi_prime = x2 - rx
        let yi = y2 + ry
        let yi_prime = y2 - ry
        Some (Point (xi, yi), Point (xi_prime, yi_prime))

let getAngles circle1 circle2 =
    intersection circle1 circle2
    |> Option.map (fun (x, y) ->
        let get circle =
            let (Circle (Point (a, b), _)) = circle
            let p = Point (a, b)
            let x2 = angle p x
            let y2 = angle p y
            Point (float x2, float y2)
        get circle1, get circle2)

let hasIntersection circle1 circle2 =
    intersection circle1 circle2
    |> Option.isSome

let removeIfParent intersections = [
    let parents = ResizeArray()
    for (circle, inter) in intersections do
        parents.Add(circle)
        let inter' = inter |> List.filter (not << parents.Contains)
        yield circle, inter'
]

let intersections circles =
    (circles, circles)
    ||> List.allPairs // all combinations of circles
    |> List.filter (fun (x, y) -> x <> y) // remove self
    |> List.groupBy fst
    |> List.map (fun (c, cs) ->
        c,
        cs
        |> List.map snd
        |> List.filter ((<>)c) // remove self
        |> List.filter (hasIntersection c)) // filter by intersection
    |> List.sortByDescending (snd >> List.length) // sort by most intersections
    |> removeIfParent // remove sub elements if element exists in parent
    |> List.filter (snd >> List.isEmpty >> not) // remove elements with empty sub circles

let rec construct = function
    | ((circle, inters)::other) ->
        Some (Element (circle, List.map (fun c ->
            let subs =
                other
                |> List.tryFind (fst >> (=)c)
                |> Option.bind (List.singleton >> construct)
            match subs with
            | Some (Element (c', _) as x) when c' = c -> x
            | Some subs -> Element (c, [subs])
            | None -> Element (c, [])
        ) inters))
    | [] -> None


open Fable.Import.Browser

let drawArc (ctx: CanvasRenderingContext2D) (color: string) (Circle(Point (x, y), r) as circle) (sa: float<radian>, ea: float<radian>) =
    do
        // cut out everything underneath
        ctx.globalCompositeOperation <- "destination-out"
        ctx.beginPath()
        ctx.fillStyle <- !^"black"
        ctx.arc(x, y, r, float sa, float ea)
        ctx.fill()
    do
        ctx.globalCompositeOperation <- "source-over"
        ctx.beginPath()
        ctx.fillStyle <- !^color
        ctx.arc(x, y, r, float sa, float ea)
        ctx.fill()
        ctx.lineWidth <- 2.0
        ctx.setLineDash(ResizeArray [|5.; 2.|])
        ctx.stroke()

let drawArcWith ctx color baseCircle circle =
    match getAngles baseCircle circle with
    | Some (_, Point (x, y)) ->
        let a1, a2 = radian (x * 1.<degree>), radian (y * 1.<degree>)
        drawArc ctx color circle (a2, a1)
    | None -> ()

let drawLine (ctx: CanvasRenderingContext2D) (Point (x1, y1)) (Point (x2, y2)) =
    ctx.beginPath()
    ctx.lineWidth <- 2.0
    ctx.setLineDash(ResizeArray [|5.; 2.|])
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.stroke()

let drawIntersection ctx circle1 circle2 =
    intersection circle1 circle2
    |> Core.Option.iter (fun (p1, p2) ->
        drawLine ctx p1 p2)

let drawAngle ctx circle1 circle2 =
    match intersection circle1 circle2 with
    | Some (x, y) ->
        let draw circle =
            let (Circle (Point (a, b), _)) = circle
            let p = Point (a, b)
            //let a1 = angle p x
            //let a2 = angle p y
            //printfn "%A" (abs a1, abs a2)
            drawLine ctx p x
            drawLine ctx p y
        draw circle1
        draw circle2
    | None -> ()

let init ccircles =
    let canvas = document.querySelector(".view") :?> HTMLCanvasElement

    let ctx = canvas.getContext_2d()

    let circles = ccircles |> List.map fst

    let intersections = intersections circles

    let result = construct intersections

    let find circle =
        ccircles |> List.find (fst >> (=)circle)

    let rec render baseCircle (Element (c, subs)) =
        let c, color = find c
        drawArcWith ctx color baseCircle c
        drawIntersection ctx baseCircle c
        for Element (c', subs') in subs do
            let circle', color' = find c'
            drawArcWith ctx color' c circle'
            drawIntersection ctx c circle'
            List.iter (render circle') subs'

    result
    |> Core.Option.iter (fun (Element (c, subs)) ->
        let circle, color = find c
        drawArc ctx color circle (0.<radian>, radian 360.<degree>)
        List.iter (render circle) subs)

let time1 = performance.now()

let ccircles = [
    Circle (Point (280., 280.), radius = 100.), "rgba(255, 0, 0, 0.5)" // "red"
    Circle (Point (200., 200.), radius =  70.), "rgba(0, 0, 255, 0.5)" //"blue"
    Circle (Point (380., 330.), radius =  40.), "rgba(0, 255, 0, 0.5)" // "green"
    Circle (Point (400., 360.), radius =  40.), "rgba(255, 255, 0, 0.5)" //"yellow"
    Circle (Point (250., 360.), radius =  50.), "rgba(255, 0, 255, 0.5)" //"magenta"
    Circle (Point (340., 220.), radius =  60.), "rgba(0, 255, 255, 0.5)" //"cyan"
]

init ccircles

let time2 = performance.now()

console.log(sprintf "Time for render: %.0f milliseconds" (time2 - time1))