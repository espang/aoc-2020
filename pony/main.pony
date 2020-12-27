use "cli"

actor Main
    new create(env: Env) =>
    let cs =
        try
            CommandSpec.leaf("aoc", "Advent of code", [
                OptionSpec.i64("day", "The day to execute"
                    where short' = 'D', default' = 1)
            ], [
                ArgSpec.string("input_file_name", "Name of the input file")
            ])?.>add_help()?
        else
            env.exitcode(1)
            return
        end

    let cmd =
        match CommandParser(cs).parse(env.args, env.vars)
        | let c: Command => c
        | let ch: CommandHelp =>
            ch.print_help(env.out)
            env.exitcode(0)
            return
        | let se: SyntaxError =>
            env.out.print(se.string())
            env.exitcode(1)
            return
        end
    
    let day = cmd.option("day").i64()
    let filename = cmd.arg("input_file_name").string()
    env.out.print("Start problem of day: " + day.string())
    match day 
    | 1 =>
        Day1(env, filename)
    | 2 =>
        Day2(env, filename)
    else
        env.out.print("No implementation for day: " + day.string())
    end