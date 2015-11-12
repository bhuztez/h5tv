"use strict";

window.addEventListener(
    "load",
    function() {
        var videoElem = document.getElementById("video");
        navigator.mediaDevices.getUserMedia(
            { video: true }
        ).then(
            function(stream) {
                var mediaRecorder = new MediaRecorder(stream);
                mediaRecorder.addEventListener(
                    "dataavailable",
                    function(event) {
                        console.log(event);
                    }
                );
                mediaRecorder.start(2000);
            }
        );
    }
);
