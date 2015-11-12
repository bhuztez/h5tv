"use strict";

function get_vint_length(c) {
    var i;
    for (i=9; c>0; i--) {
        c = c >> 1;
    }
    return i;
}

function get_vint_value(array, offset, length) {
    var v = array[offset] - (1 << (8 - length));

    for (var i=1; i<length; i++) {
        v = (v << 8) + array[offset+i];
    }

    return v;
}


function parse_header_size(buffer) {
    var view = new DataView(buffer);
    var array = new Uint8Array(buffer);
    if (view.getUint32(0, false) !== 0x1a45dfa3) {
        throw "EBML Element ID not found";
    }

    if (array[4] === 0) {
        throw "Bad EBML Size";
    }

    var length = get_vint_length(array[4]);
    var ebml_size = get_vint_value(array, 4, length);
    var segment_offset = 4 + length + ebml_size;

    if (view.getUint32(segment_offset, false) !== 0x18538067) {
        throw "Segment Element ID not found";
    }

    var length = get_vint_length(array[segment_offset+4]);
    var offset = segment_offset + 4 + length;

    while (view.getUint32(offset, false) != 0x1F43B675) {
        offset += get_vint_length(array[offset]);
        var length = get_vint_length(array[offset]);
        var size = get_vint_value(array, offset, length);
        offset += length + size;
    }

    return offset;
}


function onload() {
    var videoElem = videoElem = document.getElementById("video");
    var buffers = [];
    var paths = [];
    var uploading = false;
    var header_uploaded = false;

    function do_upload(path, data) {
        var xhr = new XMLHttpRequest();
        xhr.open("PUT", path);
        xhr.addEventListener(
            "loadend",
            function() {
                uploading = false;
                notify();
            }
        );

        xhr.send(data);
    }

    function upload() {
        uploading = true;
        var path = paths.shift();
        var buffer = buffers.shift();

        if (header_uploaded) {
            do_upload(path, buffer);
            return
        }

        header_uploaded = true;
        var reader = new FileReader();

        reader.addEventListener(
            "load",
            function() {
                var header_size = parse_header_size(this.result);
                var header_buffer = this.result.slice(0, header_size);
                buffers.unshift(this.result.slice(header_size));
                do_upload(path, header_buffer);
            }
        );

        reader.readAsArrayBuffer(buffer);
    }

    function notify() {
        if (uploading)
            return;

        if (buffers.length === 0)
            return;

        if (paths.length === 0)
            return;

        upload();
    }


    navigator.mediaDevices.getUserMedia(
        { video: true }
    ).then(
        function(stream) {
            videoElem.src = URL.createObjectURL(stream);
            videoElem.play();

            var mediaRecorder = new MediaRecorder(stream);
            mediaRecorder.addEventListener(
                "dataavailable",
                function(event) {
                    buffers.push(event.data);
                    notify();
                }
            );

            var name = window.location.search.slice(6);
            var eventSource = new EventSource("http://127.0.0.1:8001/live/" + name);
            eventSource.addEventListener(
                "open",
                function(event) {
                    mediaRecorder.start(5000);
                }
            );

            eventSource.addEventListener(
                "message",
                function(event) {
                    if (paths.indexOf(event.data) == -1) {
                        paths.push(event.data);
                        notify();
                    }
                }
            );

        }
    );
}

window.addEventListener("load", onload);
