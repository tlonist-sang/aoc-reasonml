let readFileAsLines = (filename, trim) => {
  let fileContents = Node.Fs.readFileAsUtf8Sync(filename);
  Js.String.split("\n", trim ? Js.String.trim(fileContents) : fileContents)
  ->Belt.List.fromArray;
};

let inputFilename = tag => "input/" ++ tag ++ ".txt";
let readInputLines = (~trim=true, tag) =>
  tag->inputFilename->readFileAsLines(trim);


readInputLines("q1in");
Js.log("hello!");
