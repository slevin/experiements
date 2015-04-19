module path;

import std.math;
import std.stdio;
import std.format;
import std.algorithm;

import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

struct PathLineCheckResults {
    vec2 normalPoint;
    bool onLine;
    float distance;
    bool withinRadius;
    vec2 pointAhead;
}

struct PathLine {
    vec2 start;
    vec2 end;
    float radius;
    float aheadDistance;

    PathLineCheckResults normalForPoint(vec2 p) {
        vec2 a = p - start;
        vec2 b = end - start;
        b.normalize();
        vec2 unitB = b;
        float lenB = a * b;
        b *= lenB;
        vec2 normalPoint = start + b;
        bool onLine = (normalPoint.x >= start.x && normalPoint.x <= end.x) ||
            (normalPoint.x <= start.x && normalPoint.x >= end.x);
        vec2 pn = normalPoint - p;
        float distance = (pn.x ^^ 2 + pn.y ^^ 2) ^^ 0.5;
        bool withinRadius = distance <= radius;
        vec2 pointAhead = unitB * (lenB + aheadDistance);
        return PathLineCheckResults(normalPoint,
                                    onLine,
                                    distance,
                                    withinRadius,
                                    pointAhead);
    }
}

struct PathCheckResults {
    bool needsSteering;
    vec2 steerTarget;
}

struct Path(size_t inPoints) {
    enum size_t points = inPoints;
    sfVertex[points * 2] lineVertices; // 2 points per line
    PathLine[points] pathLines;

    uint numberOfLines() {
        return points;
    }

    void fill(vec2 function(ulong index) fillFunction) {
        foreach(pt; 0 .. points) {
            auto nextPoint = pt + 1;
            if (pt == points - 1) {
                // last point wraps
                nextPoint = 0;
            }
            vec2 v1 = fillFunction(pt);
            lineVertices[pt * 2] = sfVertex(sfVector2f(v1.x, v1.y), sfBlue);
            vec2 v2 = fillFunction(nextPoint);
            lineVertices[pt * 2 + 1] = sfVertex(sfVector2f(v2.x, v2.y), sfBlue);
            pathLines[pt] = PathLine(v1, v2, 10, 25);
        }
    }

    PathCheckResults steerCheckForPoint(vec2 p) {
        auto results = map!(a => a.normalForPoint(p))(pathLines[]);
        if (any!(a => a.withinRadius)(results)) {
            return PathCheckResults(false);
        } else {
            auto best = results
                .filter!(a => a.onLine)
                .minPos!((a, b) => a.distance < b.distance);
            if (best.empty == false) {
                return PathCheckResults(true, best.front.pointAhead);
            } else {
                return PathCheckResults(false);
            }
        }
    }

    void render(sfRenderWindow *win) {
        sfRenderWindow_drawPrimitives(win,
                                      lineVertices.ptr,
                                      numberOfLines(),
                                      sfLines,
                                      null);
    }

}
