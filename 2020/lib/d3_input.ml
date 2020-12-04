let test_input = [
  "..##......."
; "#...#...#.."
; ".#....#..#."
; "..#.#...#.#"
; ".#...##..#."
; "..#.##....."
; ".#.#.#....#"
; ".#........#"
; "#.##...#..."
; "#...##....#"
; ".#..#...#.#"
]

let input1 = [
  "..#..#......#..#.......#...#.#."
; "...##.....##..#..#....#.##.##.#"
; "...#...#.##...##.....#.....#.#."
; "..#....#.....#...##.##.###.#..."
; ".#.....#......#..#.##.#...###.."
; "#..#..#.#......#...........###."
; "#......####.#....##........##.."
; ".#.......#.....#......#...#...."
; "...#...#.....#.......##.....##."
; "#...##.........#.#..##..#..#.##"
; "#.#.##.........#.#..#.#.....###"
; ".##..##...#....##.....#..#....."
; "........#.......###.#.#.....#.#"
; "...#.#....#.##..#...##..##..#.."
; "......#....#........######.#..."
; ".##...#.#...###......#.#.#..#.#"
; "........#.##...##.#...#..#...##"
; ".#..#.#..##....###..#.#.......#"
; "..#..##..#.#...#.##......#....."
; "##.....#..##.#.#..#......##...#"
; "......................#..#..#.."
; "..#.##....####.........###.##.."
; "##..###..#...#....#..#.#...#..."
; ".##.#......#..#....#........#.."
; ".#.....#..#..#.#.#....#.....##."
; "..........#..#....#..##...#..##"
; ".#...#.#....#.##..#.....#....#."
; "#..............#.#.#..#..#....#"
; "...#.#...............##........"
; "#.#.##...#.##..##.....#........"
; "...#.......###..###..#...#..#.."
; "####..#.#..##.....##.#.#......#"
; ".#.#.......#..##.......#......."
; "#....#...#.##.#.......#..#....."
; ".#...##..#..#..##.......##...#."
; ".#..#......#.........#........."
; "#.##.#.....#....#..##..#.....#."
; "#.#....#.#....#...#.#..#....#.."
; "#..#.....#.##..#.....#...##...#"
; "#....#...##.#.........#.#....##"
; ".......##.##......##.......##.."
; "#.....#..#........#........#..."
; "#....#.#..#.#........##.#...#.."
; "#.......#.#.#.#....#.......##.#"
; "...#..###..........#...#.#.###."
; "....#..#....#...#....##.#.....#"
; ".#..##.....#..#....##..##...#.#"
; "#.........#....#.#..###...##..."
; ".#.#.........#.#.......#.#.#..#"
; "..........#........##..#......."
; ".....#.......#...#.....#..##.##"
; "...#.........#.............####"
; "##..#...#..#.#......#...#......"
; ".#..###...#.#.#.#...#...#......"
; "....#..##.#....#..#.#..##..##.#"
; "..#.......#......#..#.......#.."
; "....###......#...##...#....#..."
; "..#..#.....#...#..###....#.#..#"
; ".........##..#.##....#..##..#.."
; "##...#...#.#.........##......#."
; "###..#.#....#......##..##.#...#"
; ".##...##..#.#.#.#......#..#...."
; "###......#..#..#.....#..#....#."
; ".#.#..##....##........##..#.#.."
; "###...####.#....#.......###...."
; "..#....###..#.#.#..#.......##.."
; ".......#.#...#.....#.#....##.#."
; "......#......#.#....#..##..###."
; "....####..........#.....#......"
; ".###.....#...#..#...##.#...###."
; "...##....##....###....#.#..#.#."
; "##.#..........##.........#.##.."
; "..#..#.#.###..##..#....##.....#"
; "..#....##.....#...##....###..##"
; "....#.......##..#..#..........#"
; "............#..#.###..#.#......"
; "...........##......#.#.#...#..#"
; "...##.##....#...##.##.....#.#.."
; ".####...#....###...#.....#....#"
; ".##........#..##..#.#.....#...."
; "..................#.....#..##.."
; "..###.....#.##..#..#....##...#."
; "...#.##.#.####.#.###.#....#..##"
; ".#....##..##......####.#####..."
; "#...#.#....##.........##....#.."
; "..#.##.....##.............#.##."
; "###.....#.#..#..#......#.##.#.."
; "...#..##.....#...##...#......#."
; ".##.#...#......##.#..##....#..."
; ".....##.....#......#.#........."
; "#.....#.....#........##........"
; ".#......##...#..#.#....###.#..#"
; "#.####...#....#.........#..#..."
; "#..##.#.....#.##.##.#....#...#."
; "#########..#....#..#...#......#"
; "..##..##...###.######...##.##.."
; "##.......#.......#.#....###..#."
; ".....#...#.######..#.....#....."
; ".#......#..#.............#.##.#"
; "..###.#.#......##...###........"
; ".......####.#..##....#........#"
; "..#......#.##....##.##....#...."
; "....#......#.#....#..#.#.....##"
; "####.....#....#.#......#.#.#.##"
; "#...##....#.#.##.........#....#"
; "....#..###......#......#...#..."
; ".....##.#..#..#...#..#.#.#....."
; ".##............#.....#........."
; "##...#..#.....##.#..#.........."
; "#.....#####.......#..#....#.#.."
; ".........#..#.....###........#."
; "#....#..#...###........#..#.#.."
; "...##...#..#...#.##..#........."
; ".........#.#.....#.......#...#."
; ".#.....#..####....#.##.......##"
; "...............##....##.##..##."
; "............#....#....#...##.#."
; "..#...#........#.......#..#...."
; "##....####....#.##...#..##..#.#"
; ".#.#.....#......#.#........#.#."
; "....#......#.#....##..##......."
; ".#..#.#..#..##.....#..........."
; "..#........#.##..#......##..#.."
; "...##.#...#...#..#........#...."
; "##..##....#......#...#..#.#.#.#"
; "......#.....#..#..#....#......."
; ".....##......#..#.#.##...#....."
; "...#.....#.#..........#..##...#"
; ".####.##....#...........#.....#"
; ".....###..##...#....##..#...#.."
; "..##...#.#...#..........#..#.#."
; "...#..#..............#.##.#...."
; "##.#....#...#..#....#.........."
; ".##..........#..#........#....."
; "#...#.#......#...#.....##..#..."
; ".##...#.#.#....###.####..#....#"
; ".#......#.#...#.#....#.#...#..."
; "#....##.###.............#.#...."
; "....#.###..##..##.##...##......"
; "##....#..###.##.##.....#......#"
; "..#..#..#......#..#..#........."
; "#.##......#.#....#..#..#......."
; "....#.#...#..###......##......."
; ".###.......##.......#....###..."
; "..#..#.##..#.#....#..#.#.....#."
; ".#..##.##..............#....#.."
; "#...#.#...#..#.##..##.#.#......"
; "#...#..#..##..##.###......#...."
; ".#..#.....#...#....#.....#...#."
; ".....#....#..#.....###...#.####"
; ".#.....#......#...##...#..#...."
; ".#......#............#.#......."
; "....##....#.#..#..#...#..#.#..."
; "#...#.....###...##...#.##.....#"
; ".......#.....#....#.......#...#"
; "#.......###.......#.#.........."
; "...#.#.###.#........#.###...#.."
; "....#............#....#..#....."
; "#......#.##.#...##.......#..#.#"
; ".....#....#....#.#.#...###..#.."
; ".....#.#...#...#.#..#....#.#..#"
; ".#.......#.#..#...###.......##."
; ".......#..#.##.........#......."
; ".##.#........#.##...##....#...."
; ".#....#..#...#......####...#..#"
; "...#.....#..##.#..#.#....#....#"
; "...##....#........#.#........#."
; ".....#....##..#.##..........#.."
; "#.....#.#.#......##....##.#..#."
; ".#.#.##..#.#....##.#....##....."
; ".....#.....#..#.#....#..#....##"
; "...#........#....#......###.#.."
; ".....##...#.....##.##.#.#.##..."
; "...#.....#####....##.#.#.###.#."
; ".#..#.#..##...###.........#.#.#"
; "#...#...#.#..#...#...........#."
; ".##..............#...#..#....#."
; "....###.........#.#.#....#....."
; "..#...##.#.#....##.#..#...#..#."
; "..#.....#.#......#....#......#."
; ".......##....#.#.##....#...#..#"
; "##.#.#...#..#......#..#..#....#"
; "...#.#......#............###.##"
; "..###..#..##..#...##........#.."
; ".#...#...##...#....#....##.#..#"
; "..##...####....#....#..#....#.#"
; "...#......##....#.........##.#."
; "##.#.......#..#..#............."
; "..#.#.#.#......#...#.#..##....."
; ".#..##.....###...##.#..#......#"
; "##...#..........#.####....##..."
; "#..........#...#..##....#......"
; "....##...#....#..####...#.##.##"
; ".#.######...##...#..##........."
; "....##.........#.......##.##..."
; ".#.....#.#..........##......#.."
; "...#..#.#.###..#........#.....#"
; "..##..#............##.......#.."
; "......##....##..#.##..#.......#"
; ".......##....#.......#..#...#.#"
; "#.#......#.###.....#.##........"
; ".#..##..........#..#.....#.##.."
; "..#.#...#....#.........#..##..#"
; ".#......#.......#...#..#..###.."
; "......#.##.....#.#......#....#."
; "....#....#...#.......#...##.##."
; "#....#...##...#..##........###."
; "##......#.#..#.......#.......#."
; "...##.##..#......#.###..#.#.##."
; ".............#..#.............#"
; "..#.......##..#..#....##...#..."
; "...............##..##........#."
; "##...#.##.......#....#.......##"
; "....##.##.#.#.....##.....##.##."
; "#.#......#.......#..#.#..#....."
; "....##....#.##........##.##.#.."
; "......##....#..##..#..##....##."
; ".............#.....#.......#..."
; ".......###.......#..........#.."
; "......##.#..#.....#.#...#.#...#"
; ".#...#..#..###.###...#....##..."
; "#......#..#.#...#...#.....#..##"
; ".###.....#..#.#......##..#.##.."
; ".##.#.....#..#.#..#....##......"
; "#......#..............#.....#.#"
; "...#..#....#.....#.....##.#...#"
; "......#..##..##.....#...#......"
; ".....####..#..#.##.......#..#.#"
; "###.#.#........#.......#.....##"
; "..#.#.#.#...#...#........#....#"
; "....##.#.#..#...##.....#......#"
; "#..#.##....#..#.##..####......."
; "...####.#...#......#......##..#"
; "#....#.#..###......#..#..##..#."
; "...........#....#...#......#..."
; "......###.#.....#.#....#.#...#."
; ".......#.##..............#..##."
; "..##...........#..#.#...#.....#"
; "#..#............##.........#.#."
; ".......###.#...#.#...#.#.#...#."
; "..#...##.......#..#......#.#.##"
; "#.#...#.....#...##.#.#.......##"
; ".#.#.##...#..##.#......#......."
; "#.......#.......#.#....#.....#."
; ".....#..#..#.......#..#........"
; "##...##...##......#..##.###...."
; "..#...#.###.#.###..#.....###..."
; ".....####.......#.#.....##....#"
; "....#....#.#....#...#..#.#..#.."
; "..##.....#....#.#.#.###...#...."
; "......#.#....#.#..#....#.#..#.."
; "#...#...#....#.......#......#.#"
; "#..#.#......#..#...........#.##"
; "...............#....#.....#...#"
; ".#.#.#...#.##...#.#.#..#....#.."
; "...#.#.####..##.#...##........."
; "##.........##.##.....##....#..."
; "................#...#.##.#.#..#"
; ".#..#....#...#..#..#..###.#..#."
; "...#..#.##.#.####..........#..#"
; "........#....##......#..#.#...."
; "........##.........#..#..#..#.#"
; "#......#.#...#...#...##.....#.."
; "#...#.....#..#..##.#...#.#.#..."
; "....#..##...##.....#...#.#....."
; "..#..##....#....#...#....#....."
; ".#..#...##.......###...#...#..."
; ".#......#......#..##..#..##...."
; "....##....#..#.#....#.#..##...."
; "###......#...........#.....###."
; ".....#...#..##.#..#..#.....#..#"
; "#.#....#...........#.##..#..###"
; "#....#...###.#...#..##..#.....#"
; ".#....#......##.#..#....#.#...."
; "....#.#....#..#.#....#..#..#..."
; "..#......#..#.#.#....#........."
; ".#...#.#.....#........#.#...###"
; "....#..##.......#.###....##...."
; "#.#.......#......#.###........#"
; "#.........#.....####.##..#..#.."
; ".#.#..##...#.#.....##.#.#..#..."
; ".#..#..#..#.##..#...###.#...#.."
; ".....##..##..##..#.#.#.....###."
; ".#..#...#..#......##.#........."
; "....#..##....#.##.........#...#"
; "........#...#...###.........##."
; "#.........#..##....#.#...#....."
; ".......#.......#..#.......#...."
; "#......##......#.#.##.........."
; ".#..##..####...#.....####.....#"
; "........#.#....#..##..###.#...#"
; ".#...#...#.........###..#...#.#"
; "#.........#....##...#.........."
; ".#.#....#..........#..........."
; ".#.#..........#.##.....#.##...."
; "..#....#...##..###..........##."
; ".#.#..#.##..#..#.##.##..##....."
; "........#...#....#...#.#..##..."
; "......#......##..#..#.....#..#."
; ".##.#....#...#....#...#..##..##"
; "##............#..........###..."
; "....#.......#.#..#.#####.....#."
; "#......#.....#...#........#...."
; "..##.....###..#.#.#.#....#....#"
; "#...#...#.#..#..#....#..#......"
; "......#....#...#..#....#####..."
; "....#.......##....#....##......"
; ".....##...#.##.#.....##....#..."
; ".#....###.#..##...##.##.......#"
; "....#.#.#.##.............#..##."
; "...........##......#...#.#.##.."
; "....##......#....#....##..##.#."
; ".#.#...#.....##.....#.........#"
; "#.#..........#.......#.##...#.."
; "....#.##..#.#....#.....#...#..."
; "##.............##.......#.##.#."
; "....#...#.....##...#..........#"
; "##..#...#...#.#.##...#.......##"
; "..#........#.....###...##..##.#"
; ".....#...##.#.#.##.....#...#..."
; "####.###...##..##...#..#..#..##"
; "......#..#..#.........#...#.#.."
; "....###.....##.##....#.##.....#"
]