"use strict";

function onload() {
    var videoElem = document.getElementById("video");
    var mediaSource = new MediaSource();
    videoElem.src = URL.createObjectURL(mediaSource);
    var channel = window.location.search.slice(1);
    var sourceBuffer;
    var loading = false;
    var updating = false;
    var loaded_timestamp;
    var current_timestamp;
    var buffers = [];
    var offset_loaded = false;
    var timeoffset;
    var started_play = false;
    var buffer_count = 0;

    function do_load(path) {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", path);
        xhr.responseType = "arraybuffer";
        xhr.addEventListener(
            "load",
            function() {
                loading = false;
                buffers.push(this.response);
                notify_buffer_update();
                notify_load();

                if (started_play) {
                    return;
                }

                buffer_count += 1;
                if (buffer_count > 3) {
                    videoElem.currentTime = timeoffset;
                    videoElem.play();
                    started_play = true;
                }
            }
        );
        xhr.send();
    }

    function notify_load() {
        if (loading) {
            return;
        }

        if (loaded_timestamp >= current_timestamp) {
            return;
        }

        loading = true;
        loaded_timestamp += 5;
        do_load("/" + channel + "/" + loaded_timestamp + ".webm");
    }

    function notify_buffer_update() {
        if (updating) {
            return;
        }

        if (buffers.length === 0) {
            return;
        }

        updating = true;
        var buffer = buffers.shift();
        sourceBuffer.appendBuffer(buffer);
    }

    function on_init_timestamp_load() {
        var init_timestamp = this.response;
        var xhr = new XMLHttpRequest();
        xhr.open("GET", "http://127.0.0.1:8001/timestamp");
        xhr.responseType = "json";

        xhr.addEventListener(
            "load",
            function() {
                current_timestamp = this.response - 10;
                setInterval(
                    function() {
                        current_timestamp += 5;
                        notify_load();
                    },
                    5000);

                loading = true;
                loaded_timestamp = current_timestamp - 15;
                timeoffset = loaded_timestamp - init_timestamp + 5;
                do_load("/" + channel + "/header.webm");
            }
        );
        xhr.send();
    }

    mediaSource.addEventListener(
        "sourceopen",
        function() {
            sourceBuffer = this.addSourceBuffer('video/webm; codecs="vp8,vorbis"');
            sourceBuffer.addEventListener(
                'updateend',
                function () {
                    updating = false;
                    notify_buffer_update();
                }
            );

            if (offset_loaded) {
                return;
            }

            offset_loaded = true;
            var xhr = new XMLHttpRequest();
            xhr.open("GET", "http://127.0.0.1:8001/timestamp/" + channel);
            xhr.responseType = "json";
            xhr.addEventListener("load", on_init_timestamp_load);
            xhr.send();
        }
    );
}

window.addEventListener("load", onload);
