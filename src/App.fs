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

let distance (Point (x0, y0)) (Point (x1, y1)) =
    let dx = x1 - x0
    let dy = y1 - y0
    Math.Sqrt((dy*dy) + (dx*dx))

let intersection (Circle (Point (x0, y0), r0)) (Circle (Point (x1, y1), r1)) =
    let d = distance (Point (x0, y0)) (Point (x1, y1))
    if (d > (r0 + r1)) then
        None
    else if (d < Math.Abs(r0 - r1)) then
        None
    else
        let dx = x1 - x0
        let dy = y1 - y0
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

let hasIntersection (Circle (point0, r0)) (Circle (point1, r1)) =
    distance point0 point1 < (r0 + r1)

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

let rec constructTree =
    let rec loop acc = function
        | ((circle, inters)::other) ->
            (Element (circle, List.map (fun c ->
                let subs =
                    other
                    |> List.tryFind (fst >> (=)c)
                    |> Option.toList
                    |> loop acc
                match subs with
                | [Element (c', _) as x] when c' = c -> x
                | subs -> Element (c, subs)
                | [] -> Element (c, [])
            ) inters))::acc
        | [] -> []
    loop []

open Fable.Import.Browser

let drawArc
        (ctx: CanvasRenderingContext2D)
        (color: string)
        (Circle(Point (x, y), r) as circle)
        (sa: float<radian>, ea: float<radian>) =
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
        ctx.lineWidth <- 1.0
        ctx.stroke()

let drawArcWith ctx color baseCircle circle =
    match getAngles baseCircle circle with
    | Some (_, Point (x, y)) ->
        let a1, a2 = radian (x * 1.<degree>), radian (y * 1.<degree>)
        drawArc ctx color circle (a2, a1)
    | None -> ()

let drawLine (ctx: CanvasRenderingContext2D) (Point (x1, y1)) (Point (x2, y2)) =
    ctx.beginPath()
    ctx.lineWidth <- 1.0
    ctx.setLineDash(ResizeArray [|5.; 2.|])
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.stroke()

let rec flatten (Element (circle, subs)) =
    circle::(List.collect flatten subs)

let render ccircles =
    let canvas = document.querySelector(".view") :?> HTMLCanvasElement

    let ctx = canvas.getContext_2d()

    let circles = ccircles |> List.map fst

    let intersections = intersections circles

    let result = constructTree intersections

    let find circle =
        ccircles |> List.find (fst >> (=)circle)

    let rec render baseCircle (Element (c, subs)) =
        let c, color = find c
        drawArcWith ctx color baseCircle c
        for Element (c', subs') in subs do
            let circle', color' = find c'
            drawArcWith ctx color' c circle'
            List.iter (render circle') subs'

    result
    |> List.iter (fun (Element (c, subs) as result) ->
        let circle, color = find c
        drawArc ctx color circle (0.<radian>, radian 360.<degree>)
        List.iter (render circle) subs)

    let flattenedResult = List.collect flatten result
    let loneCircles =
        circles |> List.fold (fun xs c ->
            if flattenedResult |> List.contains c |> not then
                c::xs
            else
                xs) []
    for c in loneCircles do
        let circle, color = find c
        drawArc ctx color circle (0.<radian>, radian 360.<degree>)

let time1 = performance.now()

type Input = { id: string; x: string; y: string }

let rndColor () =
    let rnd = Random()
    let next () = rnd.Next(128, 256) // only light colors
    sprintf "rgb(%i,%i,%i)" (next()) (next()) (next())

let ccircles =
    importDefault<Input array> "./circles.json"
    |> Array.map (fun i ->
        let color = rndColor ()
        Circle(Point(float i.x, float i.y), 10.), color)
    |> List.ofArray

let time2 = performance.now()

render ccircles

let time3 = performance.now()

console.log(sprintf "Time for JSON-import: %.0f milliseconds" (time2 - time1))
console.log(sprintf "Time for render: %.0f milliseconds" (time3 - time2))