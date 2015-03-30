// compile regular expressions

var numRegExp = new RegExp("^[0-9]*$");

// sound setup

$.ionSound({
    sounds: [
        {
            name: "klaxon"
        },
        {
            name: "button_tiny",
        },
        {
            name: "bell_ring",
        },
        {
            name: "cheonjae",
        },
        {
            name: "onetwothree",
        },
        {
            name: "too_noisy",
        },
        {
            name: "sit_down_quickly",
        },
        {
            name: "school_bell",
        },
        {
            name: "why_so_noisy",
        },
        {
            name: "new_cheonjae",
        }
    ],
    // volume: 0.5,
    path: "sounds/",
    preload: true
});

// Play a sound three minutes before the end of each lesson.

var loaded = new Date();
var loadedYear = loaded.getYear();
var loadedMonth = loaded.getMonth();
var loadedDay = loaded.getDay();
var lessonEndWarningTimes = [[9,  37],
                             [10, 27],
                             [11, 17],
                             [12,  7],
                             [13, 37],
                             [14, 27]];

function watchEndOfLesson ()
{
    var now = new Date();
    var hours = now.getHours()
    var minutes = now.getMinutes()
    for (var i = 0; i < lessonEndWarningTimes.length; i++)
    {
        if (lessonEndWarningTimes[i][0] == hours && lessonEndWarningTimes[i][1] == minutes)
        {
            $.ionSound.play("bell_ring");
            break;
        }
    }
}

// At the next top of the minute, start checking for three minutes
// before the end of the lesson.

var startWatching = 1000 - loaded.getMilliseconds() + 1000 * (60 - loaded.getSeconds())
window.setTimeout(function () { watchEndOfLesson(); window.setInterval(watchEndOfLesson, 60000); }, startWatching);

// random integers

// courtesy of https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random

function getRandomInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}

// famous functions from <http://www.quirksmode.org/js/cookies.html>

function createCookie(name,value,days) {
    if (days) {
        var date = new Date();
        date.setTime(date.getTime()+(days*24*60*60*1000));
        var expires = "; expires="+date.toGMTString();
    }
    else var expires = "";
    var path = $(location).attr('pathname');
    var dir = path.substring(0, path.lastIndexOf("/"));
    document.cookie = name+"="+value+expires+"; path="+dir;
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
    // reset clock because it's currently dangerous to do so when
    // ending a lesson
    timeWastingClock.reset();
    // reload, preventing Firefox from resubmitting POST data
    window.location.assign(document.URL.split("#")[0]);
}

// end a class

function endLesson()
{
    // elements
    var $points = $('#class_points');
    var $form = $('#end_of_class_form');
    var $password = $('#teachers_password');
    var $time = $('#class_time_wasted');

    var oldCookie = readCookie("class_cookie");
    // bail out if we've already started a class (the cookie will
    // always be set cos our CGI monad always sets it)
    if (oldCookie == "Nothing")
        return false;

    // validate
    var numRegExp = new RegExp("^[0-9]*$");
    if (numRegExp.test($points.val()) == false)
    {
        alert ("invalid points!");
        return false;
    }
    if ($password.val() == "")
    {
        alert ("invalid password!");
        return false;
    }

    // submit
    timeWastingClock.stop();
    // dangerous to reset clock here in case user inputs wrong
    // password: instead reset when *starting* a class
    // timeWastingClock.reset();
    $form.submit();
}

// toggle date style

// mplungjan on stack overflow: http://stackoverflow.com/a/15397495
function nth(d) {
    if(d>3 && d<21) return 'th'; // thanks kennebec
    switch (d % 10) {
    case 1:  return "st";
    case 2:  return "nd";
    case 3:  return "rd";
    default: return "th";
    }
}

function toggleDateStyle()
{
    var currentDate = $('#date').html();
    var today = new Date();

    var month = "January,February,March,April,May,June,July,August,September,October,November,December"
        .split(",")[today.getMonth()];
    var day = "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday"
        .split(",")[today.getDay()];
    var date = today.getDate();
    var British = day + " " + date + "<sup>" + nth(date) + "</sup> " + month + " " + today.getFullYear();
    var American = day + " " + month + " " + date + "<sup>" + nth(date) + "</sup>, " + today.getFullYear();

    if ($.jStorage.get("date_style", 0) == 0)
    {
        $('#date').html(American);
        $.jStorage.set("date_style", 1);
    }
    else
    {
        $('#date').html(British);
        $.jStorage.set("date_style", 0);
    }
}

// set initial date to British style
if ($.jStorage.get("date_style", 0) == 0)
    $.jStorage.set("date_style", 1);
else
    $.jStorage.set("date_style", 0);
toggleDateStyle();


// choose a student

function luckyNumber()
{
    var students = parseInt(readCookie("ss_cookie"));
    alert(getRandomInt(1, students));
}

// toggle the count-down and count-up clocks

function leftClockToggle()
{
    // determine what the current clock is by seeing what div exists
    if ($("#activity-countup").length)
        createCookie("clock_cookie", "0", 1);
    else
        createCookie("clock_cookie", "1", 1);

    // reload, preventing Firefox from resubmitting POST data
    window.location.assign(document.URL.split("#")[0]);
}

// function to make a FlipClock with a few additional features.  Not
// as neat a constructor as I would like because I don't fully
// understand how jQuery works
function MyFlipClock (jq, obj)
{
    var thisClock = new FlipClock(jq, obj);

    thisClock.go = $.proxy(function (seconds) {
        this.setTime(seconds);
        this.start();
    }, thisClock);
    thisClock.reset = $.proxy(function () {
        thisClock.stop();
        thisClock.setTime(0);
    }, thisClock);
    thisClock.custom = $.proxy(function () {
        var minutes = prompt('Number of minutes', '0');
        var seconds = prompt('Number of seconds', '0');

        // validate
        if (numRegExp.test(minutes) == false || numRegExp.test(seconds) == false)
            alert ("invalid input");
        else
            thisClock.go(parseInt(minutes) * 60 + parseInt(seconds));
    }, thisClock);

    return thisClock;
}

var timeWastingClock = MyFlipClock($('#time-wasting-clock'), {
    autoStart:false,
    callbacks:{
        interval:function () {
            $.ionSound.play("button_tiny");
            var time = timeWastingClock.getTime().time;
            $.jStorage.set("time_wasted", time);
            $("#class_time_wasted").val(time);
        }
    }
});
timeWastingClock.setTime($.jStorage.get("time_wasted", 0));

timeWastingClock.running = false;
timeWastingClock.reset = $.proxy(function () {
    if (this.getTime() != 1)
    {
        // if (confirm('Are you sure?'))
        // {
            if (this.running)
            {
                $('#timeWastingClockGo').html('Start <u>t</u>imer');
                this.stop();
                this.running = false;
            }
        $.jStorage.set('time_wasted', 0);
        $("#class_time_wasted").val(0);
        this.setTime(0);
        // }
    }
}, timeWastingClock);
timeWastingClock.toggle = $.proxy(function () {
    if (this.running)
    {
        $('#timeWastingClockGo').html('Start <u>t</u>imer');
        this.stop();
        this.running = false;
    }
    else
    {
        $('#timeWastingClockGo').html('Stop <u>t</u>imer');
        this.start();
        this.running = true;
    }
}, timeWastingClock);

var activityClock = MyFlipClock($('#activity-countdown'), {
    autoStart:false,
    countdown:true,
    callbacks:{
        stop:function () {
            $.ionSound.play("new_cheonjae");
        }
    }
});

var activityClockUp = MyFlipClock($('#activity-countup'), {
    autoStart:false,
    countdown:false
});
activityClockUp.toggle = $.proxy(function () {
    if (this.running)
    {
        $('#activityClockUpGo').html('Start stopwatch (<u>a</u>)');
        this.stop();
        this.running = false;
    }
    else
    {
        $('#activityClockUpGo').html('Stop stopwatch (<u>a</u>)');
        this.start();
        this.running = true;
    }
}, activityClockUp);
// activityClockUp.reset = $.proxy(function () {
//     if (this.getTime() != 1)
//     {
//         if (confirm('Are you sure?'))
//         {
//             if (this.running)
//             {
//                 $('#activityClockUpGo').html('Start <u>t</u>imer');
//                 this.stop();
//                 this.running = false;
//             }
//             this.setTime(0);
//         }
//     }
// }, activityClockUp);

// bind to keys

// only bind if the div exists (that is, a class is in session)
if ($("#time-wasting-clock").length)
{
    $(document).bind('keydown', 't', timeWastingClock.toggle);
    $(document).bind('keydown', 'j', timeWastingClock.toggle);
    // $(document).bind('keydown', 'space', timeWastingClock.toggle);
    $(document).bind('keydown', 'l', luckyNumber);
}
else
    $(document).bind('keydown', 'l', startLesson);

$(document).bind('keydown', 's', timeWastingClock.reset);
$(document).bind('keydown', 'r', activityClock.reset);
$(document).bind('keydown', 'c', activityClock.custom);

$(document).bind('keydown', 'z', activityClockUp.reset);
$(document).bind('keydown', 'a', activityClockUp.toggle);

$(document).bind('keydown', 'd', toggleDateStyle);

$(document).bind('keydown', '0', function (){activityClock.go(30);});
$(document).bind('keydown', '1', function (){activityClock.go(60);});
$(document).bind('keydown', '9', function (){activityClock.go(90);});
$(document).bind('keydown', '2', function (){activityClock.go(120);});
$(document).bind('keydown', '3', function (){activityClock.go(180);});
$(document).bind('keydown', '4', function (){activityClock.go(240);});
$(document).bind('keydown', '5', function (){activityClock.go(300);});
$(document).bind('keydown', '6', function (){activityClock.go(360);});
$(document).bind('keydown', '7', function (){activityClock.go(420);});
$(document).bind('keydown', '8', function (){activityClock.go(480);});

$(document).bind('keydown', 'k', function (){$.ionSound.play("klaxon");});
$(document).bind('keydown', 'o', function (){$.ionSound.play("onetwothree");});
$(document).bind('keydown', 'b', function (){$.ionSound.play("school_bell");});

// bind to buttons

$(document).ready(function(){

    $('#why-so-noisy').button();
    $('#why-so-noisy').click(function (){$.ionSound.play("why_so_noisy");});

    $('#klaxon').button();
    $('#klaxon').click(function (){$.ionSound.play("klaxon");});

    $('#bell').button();
    $('#bell').click(function (){$.ionSound.play("school_bell");});

    $('#one-two-three').button();
    $('#one-two-three').click(function (){$.ionSound.play("onetwothree");});

    $('#too-noisy').button();
    $('#too-noisy').click(function (){$.ionSound.play("too_noisy");});

    $('#sit-down-quickly').button();
    $('#sit-down-quickly').click(function (){$.ionSound.play("sit_down_quickly");});

    $('#start-lesson').button();
    $('#start-lesson').click(function (){ startLesson(); });

    $('#end-lesson').button();
    $('#end-lesson').click(function (){ endLesson(); });

    $('#lucky-number').button();
    $('#lucky-number').click(function (){ luckyNumber(); });

    $('#date-toggle').button();
    $('#date-toggle').click(function (){ toggleDateStyle(); });

    $('#leftClockToggle').button();
    $('#leftClockToggle').click(function (){ leftClockToggle(); });

    $('#activityClockUpGo').button();
    $('#activityClockUpGo').click(activityClockUp.toggle);

    $('#activityClockUpReset').button();
    $('#activityClockUpReset').click(activityClockUp.reset);

    $('#timeWastingClockGo').button();
    $('#timeWastingClockGo').click(timeWastingClock.toggle);

    $('#timeWastingClockReset').button();
    $('#timeWastingClockReset').click(timeWastingClock.reset);

    $('#activityClockReset').button();
    $('#activityClockReset').click(activityClock.reset);

    $('#activityClockCustom').button();
    $('#activityClockCustom').click(activityClock.custom);

    $('#activityClock30s').button();
    $('#activityClock30s').click(function (){activityClock.go(30);})

    $('#activityClock60s').button();
    $('#activityClock60s').click(function (){activityClock.go(60);})

    $('#activityClock90s').button();
    $('#activityClock90s').click(function (){activityClock.go(90);})

    $('#activityClock120s').button();
    $('#activityClock120s').click(function (){activityClock.go(120);})

    $('#activityClock180s').button();
    $('#activityClock180s').click(function (){activityClock.go(180);})

    $('#activityClock240s').button();
    $('#activityClock240s').click(function (){activityClock.go(240);})

    $('#activityClock300s').button();
    $('#activityClock300s').click(function (){activityClock.go(300);})

});
