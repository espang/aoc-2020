use "files"

primitive Helper
    fun parse_range (range: String): (I64, I64) =>
        let items = range.split("-")
        try
            (items(0)?.i64()?, items(1)?.i64()?)
        else
            (0, 0)
        end

    fun parse_line (line:String): (I64, I64, U8, String) =>
        let s1 = line.split(":")
        try
            let s2 = s1(0)?.split(" ")
            (let f, let t) = parse_range(s2(0)?)
            let rest = recover val s1(1)?.clone().>strip() end
            (f, t, s2(1)?.at_offset(0)?, rest)
        else
            (0, 0, ' ', "")
        end

    fun is_valid(f: I64, t: I64, c: U8, s: String): Bool =>
        var counter: I64 = 0
        for b in s.values() do
            if b == c 
            then
                counter = counter + 1
            end
        end
        (f <= counter).op_and(counter <= t)

    fun is_valid2(f: I64, t: I64, c: U8, s:String):Bool =>
        try
            let c1:U8 = s.at_offset((f - 1).isize())?
            let c2:U8 = s.at_offset((t - 1).isize())?
            (c1 == c).op_xor(c2 == c)
        else
            false
        end

actor Day2 
    new create(env: Env, filename: String) =>
        try
            let auth = env.root as AmbientAuth
            let file = File(FilePath(auth, filename)?)
            
            var valid_passwords1: I64 = 0
            var valid_passwords2: I64 = 0
            
            for line in file.lines() do
                (let f:I64, let t:I64, let c:U8, let s:String) = Helper.parse_line(consume line)
                if Helper.is_valid(f, t, c, s)
                then 
                    valid_passwords1 = valid_passwords1 + 1
                end
                if Helper.is_valid2(f, t, c, s)
                then 
                    valid_passwords2 = valid_passwords2 + 1
                end
            end
            env.out.print("part1::" + valid_passwords1.string())
            env.out.print("part2::" + valid_passwords2.string())
        else
            env.out.print("error in day2 handling")
        end
