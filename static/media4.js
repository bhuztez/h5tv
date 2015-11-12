"use strict";

window.addEventListener(
    "load",
    function() {
        var videoElem = document.getElementById("video");
        var mediaSource = new MediaSource();
        var sourceBuffer;
        videoElem.src = URL.createObjectURL(mediaSource);

        mediaSource.addEventListener(
            'sourceopen',
            function () {
                sourceBuffer = mediaSource.addSourceBuffer('video/webm; codecs="vp8,vorbis"');
                sourceBuffer.addEventListener(
                    "updateend",
                    function () {
                        videoElem.play();
                    }
                );
            }
        );

        var reader = new FileReader();
        reader.addEventListener(
            "load",
            function() {
                sourceBuffer.appendBuffer(this.result);
            }
        );

        navigator.mediaDevices.getUserMedia(
            { video: true }
        ).then(
            function(stream) {
                var mediaRecorder = new MediaRecorder(stream);
                mediaRecorder.addEventListener(
                    "dataavailable",
                    function(event) {
                        reader.readAsArrayBuffer(event.data);
                    }
                );
                mediaRecorder.start();
            }
        );
    }
);
