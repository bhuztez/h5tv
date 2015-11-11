"use strict";

var videoElem;
var mediaSource;


function Loader(channel) {
    var sourceBuffer = null;
    var loading = false;
    var updating = false;
    var started_load = false;
    var loaded_timestamp;
    var current_timestamp;
    var buffers = [];
    var started_play = false;
    var buffer_count = 0;
    var timeoffset;

    function do_load(path) {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", path);
        xhr.responseType = "arraybuffer";
        xhr.onload = function() {
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

        };
        xhr.send();
    }

    function sourceopen(event) {
        sourceBuffer = this.addSourceBuffer('video/webm; codecs="vp8,vorbis"');
        sourceBuffer.addEventListener(
            'updateend',
            function () {
                updating = false;
                notify_buffer_update();
            }
        );

        if (started_load) {
            return;
        }

        started_load = true;
        var xhr = new XMLHttpRequest();
        xhr.open("GET", "http://127.0.0.1:8001/timestamp/" + channel);
        xhr.responseType = "json";
        xhr.onload = on_first_timestamp_load;
        xhr.send();
    }

    function on_first_timestamp_load() {
        var first_timestamp = this.response;
        var xhr = new XMLHttpRequest();
        xhr.open("GET", "http://127.0.0.1:8001/timestamp");
        xhr.responseType = "json";

        xhr.onload = function() {
            current_timestamp = this.response - 10;
            setInterval(
                function() {
                    current_timestamp += 5;
                    notify_load();
                },
                5000);

            loading = true;
            loaded_timestamp = current_timestamp - 15;
            timeoffset = loaded_timestamp - first_timestamp + 5;
            do_load("/" + channel + "/header.webm");
        }
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

    return {
        sourceopen: sourceopen
    };
}


function onload() {
    var loader = Loader(window.location.search.slice(1));
    videoElem = document.getElementById("video");
    mediaSource = new MediaSource();
    videoElem.src = URL.createObjectURL(mediaSource);
    mediaSource.addEventListener('sourceopen', loader.sourceopen);
}

window.addEventListener("load", onload);
