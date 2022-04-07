let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.sample.txt")
let forms = Js.String2.split(input, "\n")

forms->Js.log
