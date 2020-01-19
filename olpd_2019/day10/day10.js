/*
 * Advent of Code 2019, one-language-per-day challenge, day 10:
 * Javascript.
 *
 * Roughly translated from my Erlang implementation.
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

function parse(s) {
    var lines = s.split(/\r?\n/)
    var set = new Set()

    for (var y = 0; y < lines.length; y++) {
        for (var x = 0; x < lines[y].length; x++) {
            if (lines[y][x] == '#')
                set.add({ x, y })
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
                dirs.push({a, b, d})
        }
    }
    return dirs
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

function pos(x, y) {
    return {x, y}
}

var asteroids = parse(ex1)
var dirs = asteroid_dirs(asteroids)
console.log(dirs)
