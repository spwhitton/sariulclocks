// famous functions from <http://www.quirksmode.org/js/cookies.html>

function createCookie(name,value,days) {
    if (days) {
        var date = new Date();
        date.setTime(date.getTime()+(days*24*60*60*1000));
        var expires = "; expires="+date.toGMTString();
    }
    else var expires = "";
    document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1,c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}

function eraseCookie(name) {
    createCookie(name,"",-1);
}

// start a new class

function startLesson()
{
    var oldCookie = readCookie("class_cookie");
    // bail out if we've already started a class (the cookie will
    // always be set cos our CGI monad always sets it)
    if (oldCookie != "Nothing")
        return false;

    // get input
    var theGrade = prompt("What grade?", "");
    var theClass = prompt("What class?", "");
    cookieString = theGrade + "-" + theClass

    // validate
    var valRegExp = new RegExp("^[0-9]-[0-9]$");
    if (valRegExp.test(cookieString) == false)
    {
        alert ("invalid class!");
        return false;
    }

    // set the cookie and reload to start the session
    createCookie("class_cookie", cookieString, 1);
    location.reload(true);
}

// toggle the count-down and count-up clocks

function leftClockToggle()
{
    // determine what the current clock is by seeing what div exists
    if ($("#activity-countup").length)
        createCookie("clock_cookie", "0", 1);
    else
        createCookie("clock_cookie", "1", 1);
    location.reload(true);
}

// bind to buttons

$(document).ready(function(){
    $('#start-lesson').button();
    $('#start-lesson').click(function (){ startLesson(); });

    $('#leftClockToggle').button();
    $('#leftClockToggle').click(function (){ leftClockToggle(); });
});
