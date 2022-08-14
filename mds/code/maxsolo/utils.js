/**
* MAXCHAT utility functions
* 
* @spartacusrex
*/

const utf8encoder = new TextEncoder();

function hexToUtf8(s)
{
  return decodeURIComponent(
     s.replace(/\s+/g, '') // remove spaces
      .replace(/[0-9A-F]{2}/g, '%$&') // add '%' before each 2 characters
  );
}

function utf8ToHex(s)
{
  const rb = utf8encoder.encode(s);
  let r = '';
  for (const b of rb) {
    r += ('0' + b.toString(16)).slice(-2);
  }
  return r;
}

function showDiv(id,show) {
  var x = document.getElementById(id);
  if (show) {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}

function showNotification(from, message){
	
	MDS.log("NOTIFICATION : "+message+" permission:"+Notification.permission+" visibility:"+document.visibilityState);
	
	//Only show if we can''t see it..
	if(document.visibilityState !== "visible") {
	     
		// If it's okay let's create a notification
		if (Notification.permission === "granted") {
	       	const notification = new Notification(from,{
			  body: message,
			  icon: './minimalogo.png'
			});
	    }
	}
	
}

function checkImageFile(input){
	
	//Get the chosen file..
	file = input.files[0];
	
	//What to do when file is loaded	
	let reader = new FileReader();
	reader.onload = function() {
		
		var image = new Image();
        image.onload = function (imageEvent) {

            // Resize the image
            var canvas = document.createElement('canvas'),
                max_size = 300,
                width = image.width,
                height = image.height;
			
			//New width and height
			if(width>300 || height>300){
	            if (width > height) {
	                if (width > max_size) {
	                    height *= max_size / width;
	                    width = max_size;
	                }
	            } else {
	                if (height > max_size) {
	                    width *= max_size / height;
	                    height = max_size;
	                }
	            }
			}
			
			//Set the size and draw
            canvas.width  = width;
            canvas.height = height;
            canvas.getContext('2d').drawImage(image, 0, 0, width, height);
            
			//Send this RESIZED image
			sendImage(canvas.toDataURL("image/jpeg"));
        }

		//Set the Image src
        image.src = reader.result;
	};
	
	reader.onerror = function() {
		console.log(reader.error);
	};

	//Read the file..
	reader.readAsDataURL(file);
}



