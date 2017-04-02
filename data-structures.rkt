#lang racket

(provide (all-defined-out))

(struct camera (position direction width height fov))

(struct scene (camera objects))