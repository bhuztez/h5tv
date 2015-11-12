"use strict";

window.addEventListener(
    "load",
    function() {
        var videoElem = document.getElementById("video");
        navigator.mediaDevices.getUserMedia(
            { video: true }
        ).then(
            function(stream) {
                videoElem.src = URL.createObjectURL(stream);
                videoElem.play();
            }
        );
    }
);
