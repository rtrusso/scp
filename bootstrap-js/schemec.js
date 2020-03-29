var fs = require("fs")

var stream;
stream = fs.createReadStream("../src/out/scheme-compiler-flat-ts.scm");
stream.on("data", function(data) {
    var chunk = data.toString();
    console.log(chunk);
});
