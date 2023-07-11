let touchArea = document.getElementById("grid");
//let output = document.getElementById("output");

//Initial mouse X and Y positions are 0
let mouseX,
  initialX = 0;
let mouseY,
  initialY = 0;
let isSwiped;
//Events for touch and mouse
let events = {
  mouse: {
    down: "mousedown",
    move: "mousemove",
    up: "mouseup",
  },
  touch: {
    down: "touchstart",
    move: "touchmove",
    up: "touchend",
  },
};
let deviceType = "";
//Detect touch device
const isTouchDevice = () => {
  try {
    //We try to create TouchEvent (it would fail for desktops and throw error)
    document.createEvent("TouchEvent");
    deviceType = "touch";
    return true;
  } catch (e) {
    deviceType = "mouse";
    return false;
  }
};
//Get left and top of touchArea
let rectLeft = touchArea.getBoundingClientRect().left;
let rectTop = touchArea.getBoundingClientRect().top;
//Get Exact X and Y position of mouse/touch
const getXY = (e) => {
  mouseX = (!isTouchDevice() ? e.pageX : e.touches[0].pageX) - rectLeft;
  mouseY = (!isTouchDevice() ? e.pageY : e.touches[0].pageY) - rectTop;
};
isTouchDevice();
//Start Swipe
touchArea.addEventListener(events[deviceType].down, (event) => {
  isSwiped = true;
  //Get X and Y Position
  getXY(event);
  initialX = mouseX;
  initialY = mouseY;
});
//Mousemove / touchmove
touchArea.addEventListener(events[deviceType].move, (event) => {
  if (!isTouchDevice()) {
    event.preventDefault();
  }
  if (isSwiped) {
    getXY(event);
    let diffX = mouseX - initialX;
    let diffY = mouseY - initialY;

	console.log("diffx:"+diffx);
	console.log("diffx:"+diffy);    

	/*if (Math.abs(diffY) > Math.abs(diffX)) {
      	
		output.innerText = diffY > 0 ? "Down" : "Up";
    } else {
      	output.innerText = diffX > 0 ? "Right" : "Left";
    }*/
  }
});
//Stop Drawing
touchArea.addEventListener(events[deviceType].up, () => {
  isSwiped = false;
});
touchArea.addEventListener("mouseleave", () => {
  isSwiped = false;
});
window.onload = () => {
  isSwiped = false;
};