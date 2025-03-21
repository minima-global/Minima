var determineFontHeight = function(fontStyle) {
    var body = document.getElementsByTagName("body")[0];
    var dummy = document.createElement("div");
    var dummyText = document.createTextNode("M");
    dummy.appendChild(dummyText);
    dummy.setAttribute("style", fontStyle);
    body.appendChild(dummy);
    var result = dummy.offsetHeight;
    body.removeChild(dummy);
    return result;
};

function getTextWidth(font) {
    var canvas = getTextWidth.canvas || (getTextWidth.canvas = document.createElement("canvas"));
    var context = canvas.getContext("2d");
    context.font = font;
    var metrics = context.measureText("M");
    return metrics.width;
}

function calculateRows() {
    var family = term.getOption('fontFamily');
    var size = term.getOption('fontSize');
    style = "font-family: " + family + "; font-size: " + size + "px;";
    rowHeight = determineFontHeight(style);

    return Math.round(window.innerHeight/rowHeight);
}

function calculateCols() {
    var family = term.getOption('fontFamily');
    var size = term.getOption('fontSize');
    var font = size + "px " + family;
    colWidth = getTextWidth(font);

    return Math.round(window.innerWidth/colWidth);
}