var isSet = function(variable){
    return typeof variable !== typeof undefined ? true : false;
};

var Cookie = {
  status: false,
  name: null,
  setCookie: function(name, value, time){
    value = encodeURIComponent(value);
    if(!isSet(time))
    {
      var time = (365*24*60*60*1000);
    }
    var data = new Date();
    data.setTime(data.getTime() + time);
    var expires = "; expires="+data.toGMTString();
    document.cookie = name + "=" + value + expires + "; path=/";
  }, 
  getCookie: function(name){
    var cookies=document.cookie.split("; "); 
    for (var i=0; i<cookies.length; i++) 
      { 
        var cookieName=cookies[i].split("=")[0]; 
        var cookieVal=cookies[i].split("=")[1]; 
        if (cookieName===name) 
          {
            return decodeURI(cookieVal);
          }
       }   
  },
  checkName: function(name){
    var cookies=document.cookie.split("; "); 
    for (var i=0; i<cookies.length; i++) 
      { 
        var cookieName=cookies[i].split("=")[0]; 
        var cookieVal=cookies[i].split("=")[1]; 
        if (cookieName===name) 
          {
            return true;
          }
      }
    return false;
  },
  removeCookie: function(name){
    Cookie.setCookie(name, null, -1);
  } 
}; 
 
var StatusBar = function(){
  var value = document.querySelector('main');
  var line = this.value.substr(0,this.selectionStart).split("\n").length;
  var letters = this.value.split("\n")[line-1].length;
  document.querySelector('#line_number').innerHTML = line;
  document.querySelector('#letters_number').innerHTML = letters;
};

var ShowHide = function (element)
{
  element = document.querySelector(element);
  if(element.classList.contains("hide"))
  {
      element.classList.remove('hide');
      element.classList.add('show');
  }
  else
  {
      element.classList.remove('show');
      element.classList.add('hide');
  }
}

var CreateNewAlert = function(title, message, time){
  if(!isSet(time))
  {
    time = 2000;      
  }
  var alert = document.querySelector('.alert');
  alert.childNodes[1].innerHTML = title;
  alert.childNodes[3].innerHTML = message;
  ShowHide('.alert');
  setTimeout(function(){ShowHide('.alert');}, time);
}

var CloseDialogWindow = function(element, id){
  element.addEventListener('click', function(){
      ShowHide('#' + id);
    }, false); 
};

var CreateDialogWindow = function(){
  element = document.querySelectorAll('.dialog-window');
  for(var x=0;x<element.length;x++)
  {
    if(element[x].classList.contains("auto-height"))
    {
      element[x].classList.add('dialog-auto-height');
    }
    element[x].classList.add('flex', 'dialog', 'hide');
    var title = element[x].childNodes[1].innerHTML;
    var content = element[x].childNodes[3].innerHTML;
    element[x].innerHTML = '<div><div class="flex"><div>'+title+'</div><span class="fa fa-times exit-dialog-window" aria-hidden="true"></span></div><div>'+content+'</div></div>';
    CloseDialogWindow(element[x].querySelector('.exit-dialog-window'), element[x].id);
  }
};   

var ChangeFileName = function(name){
  Cookie.status = true;
  Cookie.name = name;
  document.querySelector('.title').innerHTML = name;
};

var RemoveFileName = function(){
  Cookie.status = false;
  Cookie.name = null;
  document.querySelector('.title').innerHTML = 'Untitled';
};

var SaveFile = function(name) {
  var text = document.querySelector('#main').value;
  var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
  saveAs(blob, name + ".txt");
};

var LoadCookie = function(){
  var cookies=document.cookie.split(";"); 
  var dialog = document.querySelector('.open_cookie_div');
  dialog.innerHTML = null;
  for (var i=0; i<cookies.length; i++) 
    {  
      var name = cookies[i].split("=")[0].trim(); 
      if (name == 'split' || name == 'Hm_lvt_46ff8155ad9e0eabf41b044cbd4132b4' || name == 'pgv_pvi' || name == '_gat' || name == '_ga') continue;  
      var element = document.createElement('div');
      element.innerHTML = '<span>'+name+'</span><span class="fa fa-times" aria-hidden="true"></span>';   
      element.classList.add('open_cookie_option_div');
      dialog.appendChild(element);
      element.childNodes[0].addEventListener('click', function(){
        name = this.innerHTML;
        Cookie.setStatus = true; 
        Cookie.name = name;
        ChangeFileName(name);
        document.querySelector('#main').value = Cookie.getCookie(name);
        ShowHide('#open_cookie_dialog');
        CreateNewAlert('Information', 'File has been loaded');
      }, false);
      element.childNodes[1].addEventListener('click', function(){
        if(confirm('Do you want to remove this cooke file'))
          { 
            name = this.previousSibling.innerHTML;
            Cookie.removeCookie(name);
            LoadCookie();
          }
        else
          {
            return null;
          }
      }, false);
     } 
}; 

var NewFile = function(){
  Cookie.status = false;
  Cookie.name = null;
  document.querySelector('.title').innerHTML = 'Untitled';
  document.querySelector('#main').value = null;
  CreateNewAlert('Information', 'File has been created');
}

CreateDialogWindow();
LoadCookie();

document.querySelector('#save_cookie').addEventListener('click', function(){
  if(Cookie.status === true)
    {
      var name = Cookie.name;
      Cookie.removeCookie(name);
      var value = document.querySelector('#main').value;
      Cookie.setCookie(name, value);
      CreateNewAlert('Information', 'File is saved as '+name);
    }
  else
    { 
      ShowHide('#save_cookie_dialog');
    }
}, false);

document.querySelector('#save_cookie_form').addEventListener('submit', function(e){
  e.preventDefault();
  var name = this.elements[0].value;
  var value = document.querySelector('#main').value;
  if(Cookie.checkName(name) === true)
    {
      if(confirm('This cookie is exist. Do you want to overwrite?'))
        {
          Cookie.removeCookie(name);
          Cookie.setCookie(name, value);
        }
      else
        {
          return null;
        }
    }
  else
    {
      Cookie.setCookie(name, value);
    }
  ShowHide('#save_cookie_dialog');
  ChangeFileName(name);
  this.reset();
  CreateNewAlert('Information', 'File is saved as '+name);
}, false);

document.querySelector('#save_file_form').addEventListener('submit', function(e){
  e.preventDefault();
  var name = this.elements[0].value;
  SaveFile(name);
  ShowHide('#save_file_dialog');
  this.reset();
  CreateNewAlert('Information', 'File is saved as '+name);
}, false);

document.querySelector('#open_file').addEventListener('change', function(){
  var file = this.files[0];
  var reader = new FileReader();
  reader.onload = function() {
    document.querySelector('#main').value = reader.result;
  }
  reader.readAsText(file);
  CreateNewAlert('Information', 'File has been loaded');
}, false); 

document.querySelector('#open_file_button').addEventListener('click', function(){
  document.querySelector('#open_file').click();
}, false);

document.body.addEventListener('keydown', function(e){
  if((e.which == '115' || e.which == '83' ) && (e.ctrlKey || e.metaKey) && !(e.altKey))
  { 
    e.preventDefault();
    document.querySelector('#save_cookie').click();
  }
}, false); 

document.querySelector('#copy').addEventListener('click', function(){
  document.querySelector('#main').select();
  document.execCommand('copy');
}, false);

document.querySelector('#paste').addEventListener('click', function(){
  document.querySelector('#main').select();
  var a = document.execCommand('paste');
  
}, false);


document.querySelector('#main').addEventListener('keyup', StatusBar, false);
document.querySelector('#main').addEventListener('keydown', StatusBar, false);
document.querySelector('#main').addEventListener('click', StatusBar, false);
document.querySelector('#option_statusbar').addEventListener('click', function(){ShowHide('.status-bar');}, false); 
document.querySelector('#save_file').addEventListener('click', function(){ShowHide('#save_file_dialog');}, false); 
document.querySelector('#open_cookie_button').addEventListener('click', function(){ShowHide('#open_cookie_dialog');LoadCookie();}, false); 
document.querySelector('#new_file').addEventListener('click', NewFile, false);
