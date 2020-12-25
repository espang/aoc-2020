let in1 = 1717001UL
let in2 = 523731UL

let rec step acc n subjectNumber =
    if n = 0
    then acc
    else step (acc * subjectNumber % 20201227UL) (n - 1) subjectNumber


let doUntil number subjectNumber =
    let rec loop acc value =
        if value = number
        then acc
        else loop (acc + 1) (value * subjectNumber % 20201227UL)

    loop 0 1UL

let secretLoopSize1 = doUntil in1 7UL
let secretLoopSize2 = doUntil in2 7UL

let key1 = step 1UL secretLoopSize1 in2
let key2 = step 1UL secretLoopSize2 in1
