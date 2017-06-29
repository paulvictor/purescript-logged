var logStr = function(){
  return "A 5\nB 6"
}

var emptyLog = function(){
}

var writeLog = function(s){
  return function(){
  }
}

exports.logStr = logStr
exports.emptyLog = emptyLog
exports.writeLog = writeLog
