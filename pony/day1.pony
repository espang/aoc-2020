use "files"
use "collections"


actor Day1
    let _env: Env

    new create (env:Env, filename: String) =>
        _env = env
        try
            let auth = env.root as AmbientAuth
            let file = recover File(FilePath(auth, filename)?) end
            let r = Reader(env, consume file)
            r.calculate(this)
            r.calculate2(this)
        else
            _env.out.print("error in main")
        end

    be display(n: I64) =>
        _env.out.print("part1::" + (n * (2020-n)).string())

    be display2(n1: I64, n2: I64) =>
        _env.out.print("part2::" + (n1 * n2 * (2020-n1-n2)).string())

    be nothing_found() =>
        _env.out.print("no solution found")

actor Reader 
    let _file: File
    let _values: Array[I64]

    new create (env: Env, file: File iso) =>
        _file = consume file
        let values = Array[I64](200)
        for line in _file.lines() do
            try
                let nbr = line.i64()?
                values.push(nbr)
            end
        end
        _values = values

    be calculate(main: Day1) =>
        let values = Set[I64].create()
        for v in _values.values() do
            if values.contains(2020 - v)
            then
                main.display(v)
                return
            else
                values.set(v)
            end
        end
        main.nothing_found()

    be calculate2(main: Day1) =>
        let solutions = Set[I64].create()
        for s1 in _values.values() do
            for s2 in _values.values() do
                if solutions.contains(2020 - s1 - s2)
                then 
                    main.display2(s1, s2)
                    return
                else
                    solutions.set(s1)
                end
            end
        end
        main.nothing_found()
