/*
 * Advent of Code 2019, one-language-per-day challenge, day 10:
 * Javascript.
 */

var ex1 = `.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
`

var input = `#..#....#...#.#..#.......##.#.####
#......#..#.#..####.....#..#...##.
.##.......#..#.#....#.#..#.#....#.
###..#.....###.#....##.....#...#..
...#.##..#.###.......#....#....###
.####...##...........##..#..#.##..
..#...#.#.#.###....#.#...##.....#.
......#.....#..#...##.#..##.#..###
...###.#....#..##.#.#.#....#...###
..#.###.####..###.#.##..#.##.###..
...##...#.#..##.#............##.##
....#.##.##.##..#......##.........
.#..#.#..#.##......##...#.#.#...##
.##.....#.#.##...#.#.#...#..###...
#.#.#..##......#...#...#.......#..
#.......#..#####.###.#..#..#.#.#..
.#......##......##...#..#..#..###.
#.#...#..#....##.#....#.##.#....#.
....#..#....##..#...##..#..#.#.##.
#.#.#.#.##.#.#..###.......#....###
...#.#..##....###.####.#..#.#..#..
#....##..#...##.#.#.........##.#..
.#....#.#...#.#.........#..#......
...#..###...#...#.#.#...#.#..##.##
.####.##.#..#.#.#.#...#.##......#.
.##....##..#.#.#.......#.....####.
#.##.##....#...#..#.#..###..#.###.
...###.#..#.....#.#.#.#....#....#.
......#...#.........##....#....##.
.....#.....#..#.##.#.###.#..##....
.#.....#.#.....#####.....##..#....
.####.##...#.......####..#....##..
.#.#.......#......#.##..##.#.#..##
......##.....##...##.##...##......
`

function parse(s) {
    var lines = s.split(/\r?\n/)
    var set = new Set()

    for (var y = 0; y < lines.length; y++) {
        for (var x = 0; x < lines[y].length; x++) {
            if (lines[y][x] == '#')
                set.add({x: x, y: y})
        }
    }

    return set
}

function asteroid_dirs(set) {
    var dirs = []
    for (var it0 = set.values(), a = null; a = it0.next().value; ) {
        for (var it1 = set.values(), b = null; b = it1.next().value; ) {
            var d = direction(a, b)
            if (a != b)
                dirs.push({a: a, b: b, dir: d})
        }
    }
    return dirs
}

function asteroids_by_visibility(dirs) {
    var map = new Map()

    for (var i = 0; i < dirs.length; i++) {
        var dist = distance(dirs[i].a, dirs[i].b)
        var a = dirs[i].a
        var b = dirs[i].b
        var d = dirs[i].dir
        if (!map.has(a)) {
            map.set(a, new Map())
        }
        var m = map.get(a)
        if (!m.has(d)) {
            m.set(d, [])
        }
        var md = m.get(d)
        md.push({dist: dist, b: b})
    }

    return map
}

function find_best_location(map) {
    var max_visible_asteroids = 0
    var best
    for (const [key, value] of map.entries()) {
        if (value.size > max_visible_asteroids) {
            max_visible_asteroids = value.size
            best = key
        }
    }
    return {asteroid: best, visible_asteroids: max_visible_asteroids}
}

function f_to_i(f) {
    return Math.floor(f * 1000000)
}

function direction(p0, p1) {
    var dx = p1.x - p0.x
    var dy = p1.y - p0.y
    var pi = Math.PI
    var z = Math.atan2(-dy, dx) + (3*pi)/2
    var z0 = (z > 2*pi) ? (z - 2*pi) : z
    return f_to_i(2*pi - z0)
}

function distance(a, b) {
    var dx = b.x - a.x
    var dy = b.y - a.y
    return Math.sqrt(Math.pow(dx, 2) +
                     Math.pow(dy, 2))
}

function check(expected, actual) {
    if (expected != actual) {
        console.log(`Expected ${expected}, got ${actual}`)
        process.exit(1)
    }
}

var asteroids = parse(input)
var dirs = asteroid_dirs(asteroids)
var by_visibility = asteroids_by_visibility(dirs)

// Part 1
var part1 = find_best_location(by_visibility)
check(334, part1.visible_asteroids)

function get_closest(asteroids)
{
    var n = 100000000
    var closest
    for (var i = 0; i < asteroids.length; i++) {
        var astr = asteroids[i]
        if (astr.dist < n) {
            n = astr.dist
            closest = astr
        }
    }
    return closest
}

// Part 2
var visible = by_visibility.get(part1.asteroid)
var directions = Array.from(visible.keys()).sort((a, b) => a - b)
d = directions[199]
var astr = get_closest(visible.get(d))
var part2 = astr.b.x * 100 + astr.b.y
check(1119, part2)
