"use strict";

window.addEventListener(
    "load",
    function() {
        var videoElem = document.getElementById("video");
        var mediaSource = new MediaSource();
        var sourceBuffer;
        var buffers = [];
        var updating = false;
        var buffer_count = 0;
        var started_play = false;
        videoElem.src = URL.createObjectURL(mediaSource);

        function notify() {
            if ((updating) || (buffers.length === 0)) {
                return;
            }
            updating = true;
            var buffer = buffers.shift();
            sourceBuffer.appendBuffer(buffer);
        }

        mediaSource.addEventListener(
            'sourceopen',
            function () {
                sourceBuffer = mediaSource.addSourceBuffer('video/webm; codecs="vp8,vorbis"');
                sourceBuffer.addEventListener(
                    "updateend",
                    function () {
                        updating = false;
                        if (!started_play) {
                            buffer_count += 1;
                            if (buffer_count > 2) {
                                started_play = true;
                                videoElem.play();             
                            }
                        }
                        notify();
                    }
                );
            }
        );

        var reader = new FileReader();
        reader.addEventListener(
            "load",
            function() {
                buffers.push(this.result);
                notify();
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
                mediaRecorder.start(2000);
            }
        );
    }
);
