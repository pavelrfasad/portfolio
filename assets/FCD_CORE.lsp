(vl-load-com)

; ============================================================
; FCD — Facade CAD tools
; Version: 0.8
;
; Current module:
; - settings
; - service layers
; - preview cleanup
; - orthogonal grid segment reading
; - real cell detection without completing the grid
; - AXIS / EDGES panel size modes
; - min / max panel size validation
; - facade boundary / openings filtering by layers
; - opening intersection classification with cut touch tolerance: RECT / L_CUT / T_CUT / U_CUT_RESERVED / UNSUPPORTED
; - reads all selected LINE/LWPOLYLINE objects as grid, from all layers
; - FCD_BOUNDARY and FCD_OPENINGS are both constraints and can also participate as grid edges
; - supports orthogonal LWPOLYLINE contours as grid segment sources
; - opening constraints use polygon checks, not only bounding boxes
;
; Commands:
; - FCD_SETUP
; - FCD_INITLAYERS
; - FCD_CLEAR_PREVIEW
; - FCD_GRID_PREVIEW
; - FCD_SCAN_GEOMETRY
;
; Preview classes:
; - FCD_PREVIEW_OK      : normal rectangular panels
; - FCD_PREVIEW_WARN    : too small panels
; - FCD_PREVIEW_ERR     : unsupported / outside / oversized / void
; - FCD_PREVIEW_LCUT    : panels with corner L-cut
; - FCD_PREVIEW_TCUT    : panels with side T-cut
;
; Notes:
; - Currently supports only LINE objects aligned to X/Y.
; - Rectangles are created only when all 4 real sides exist.
; - Oversized panels are marked fully red with a cross.
; - Boundary/openings are read from closed LWPOLYLINE objects on FCD_BOUNDARY/FCD_OPENINGS.
; ============================================================


; -----------------------------
; Global settings
; -----------------------------

(setq *fcd-settings*
  '(
    (mode . "EDGES")            ; AXIS / EDGES
    (jointWidth . 10.0)         ; mm, used in AXIS mode
    (minPanelWidth . 80.0)      ; mm
    (minPanelHeight . 80.0)     ; mm
    (maxPanelWidth . 999999.0)  ; mm
    (maxPanelHeight . 999999.0) ; mm
    (tol . 0.01)                ; geometry tolerance
    (cutTouchTol . 20.0)         ; mm, tolerance for recognizing cut touching panel sides
    (blockRect . "FCD_PANEL_RECT")
    (blockLCut . "FCD_PANEL_LCUT")
    (blockTCut . "FCD_PANEL_TCUT")
    (blockReveal . "FCD_REVEAL")
    (blockSill . "FCD_SILL")
    (markPrefix . "F1-P-")
  )
)


(defun fcd:get (key / pair)
  (setq pair (assoc key *fcd-settings*))
  (if pair
    (cdr pair)
    nil
  )
)


(defun fcd:set (key value)
  (if (assoc key *fcd-settings*)
    (setq *fcd-settings*
      (subst
        (cons key value)
        (assoc key *fcd-settings*)
        *fcd-settings*
      )
    )
    (setq *fcd-settings*
      (append *fcd-settings* (list (cons key value)))
    )
  )
  value
)


; -----------------------------
; Helpers
; -----------------------------

(defun fcd:near= (a b tol)
  (<= (abs (- a b)) tol)
)


(defun fcd:strict-between (v a b tol)
  (and
    (> v (+ (min a b) tol))
    (< v (- (max a b) tol))
  )
)


(defun fcd:add-unique-number (value lst tol / exists)
  (setq exists nil)

  (foreach x lst
    (if (fcd:near= value x tol)
      (setq exists T)
    )
  )

  (if exists
    lst
    (cons value lst)
  )
)


(defun fcd:sort-numbers (lst)
  (vl-sort lst '<)
)


(defun fcd:variant-point->list (v)
  (vlax-safearray->list (vlax-variant-value v))
)


(defun fcd:msg (txt)
  (princ (strcat "\n[FCD] " txt))
)


; -----------------------------
; Layers
; -----------------------------

(defun fcd:ensure-layer (layerName colorIndex / acad doc layers layerObj)
  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad))
  (setq layers (vla-get-Layers doc))

  (if (not (tblsearch "LAYER" layerName))
    (progn
      (setq layerObj (vla-Add layers layerName))
      (vla-put-Color layerObj colorIndex)
    )
    (progn
      (setq layerObj (vla-Item layers layerName))
      (vla-put-Color layerObj colorIndex)
    )
  )

  layerObj
)


(defun fcd:init-layers ()
  (fcd:ensure-layer "FCD_BOUNDARY" 7)
  (fcd:ensure-layer "FCD_OPENINGS" 1)
  (fcd:ensure-layer "FCD_GRID_AXIS" 8)
  (fcd:ensure-layer "FCD_GRID_EDGE" 8)
  (fcd:ensure-layer "FCD_PREVIEW_OK" 3)
  (fcd:ensure-layer "FCD_PREVIEW_WARN" 2)
  (fcd:ensure-layer "FCD_PREVIEW_ERR" 1)
  (fcd:ensure-layer "FCD_PREVIEW_LCUT" 4)
  (fcd:ensure-layer "FCD_PREVIEW_TCUT" 6)
  (fcd:ensure-layer "FCD_PREVIEW_VOID" 8)
  (fcd:ensure-layer "FCD_PREVIEW_UCUT" 200)
  (fcd:ensure-layer "FCD_PANELS" 4)
  (fcd:ensure-layer "FCD_TRIMS" 5)

  (fcd:msg "Служебные слои созданы/проверены.")
  (princ)
)


(defun c:FCD_INITLAYERS ()
  (fcd:init-layers)
  (princ)
)


; -----------------------------
; Delete preview
; -----------------------------

(defun fcd:delete-layer-objects (layerName / ss i ent)
  (setq ss (ssget "X" (list (cons 8 layerName))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (entdel ent)
        (setq i (1+ i))
      )
    )
  )
)


(defun fcd:clear-preview ()
  (fcd:delete-layer-objects "FCD_PREVIEW_OK")
  (fcd:delete-layer-objects "FCD_PREVIEW_WARN")
  (fcd:delete-layer-objects "FCD_PREVIEW_ERR")
  (fcd:delete-layer-objects "FCD_PREVIEW_LCUT")
  (fcd:delete-layer-objects "FCD_PREVIEW_TCUT")
  (fcd:delete-layer-objects "FCD_PREVIEW_VOID")
  (fcd:delete-layer-objects "FCD_PREVIEW_UCUT")
  (fcd:msg "Превью очищено.")
  (princ)
)


(defun c:FCD_CLEAR_PREVIEW ()
  (fcd:clear-preview)
  (princ)
)


; -----------------------------
; Settings command
; -----------------------------

(defun c:FCD_SETUP (/ mode joint minW minH maxW maxH cutTol prefix)

  (fcd:init-layers)

  (initget "Axis Edges")
  (setq mode
    (getkword
      (strcat
        "\nРежим сетки [Axis=оси швов/Edges=границы панелей] <"
        (fcd:get 'mode)
        ">: "
      )
    )
  )

  (cond
    ((= mode "Axis") (fcd:set 'mode "AXIS"))
    ((= mode "Edges") (fcd:set 'mode "EDGES"))
  )

  (setq joint
    (getreal
      (strcat
        "\nШирина шва, мм <"
        (rtos (fcd:get 'jointWidth) 2 2)
        ">: "
      )
    )
  )
  (if joint (fcd:set 'jointWidth joint))

  (setq minW
    (getreal
      (strcat
        "\nМинимальная ширина панели, мм <"
        (rtos (fcd:get 'minPanelWidth) 2 2)
        ">: "
      )
    )
  )
  (if minW (fcd:set 'minPanelWidth minW))

  (setq minH
    (getreal
      (strcat
        "\nМинимальная высота панели, мм <"
        (rtos (fcd:get 'minPanelHeight) 2 2)
        ">: "
      )
    )
  )
  (if minH (fcd:set 'minPanelHeight minH))

  (setq maxW
    (getreal
      (strcat
        "\nМаксимальная ширина панели, мм <"
        (rtos (fcd:get 'maxPanelWidth) 2 2)
        ">: "
      )
    )
  )
  (if maxW (fcd:set 'maxPanelWidth maxW))

  (setq maxH
    (getreal
      (strcat
        "\nМаксимальная высота панели, мм <"
        (rtos (fcd:get 'maxPanelHeight) 2 2)
        ">: "
      )
    )
  )
  (if maxH (fcd:set 'maxPanelHeight maxH))

  (setq cutTol
    (getreal
      (strcat
        "\nДопуск касания выреза к стороне панели, мм <"
        (rtos (fcd:get 'cutTouchTol) 2 2)
        ">: "
      )
    )
  )
  (if cutTol (fcd:set 'cutTouchTol cutTol))

  (setq prefix
    (getstring T
      (strcat
        "\nПрефикс маркировки <"
        (fcd:get 'markPrefix)
        ">: "
      )
    )
  )
  (if (/= prefix "") (fcd:set 'markPrefix prefix))

  (fcd:msg
    (strcat
      "Настройки: mode="
      (fcd:get 'mode)
      ", joint="
      (rtos (fcd:get 'jointWidth) 2 2)
      ", minW="
      (rtos (fcd:get 'minPanelWidth) 2 2)
      ", minH="
      (rtos (fcd:get 'minPanelHeight) 2 2)
      ", maxW="
      (rtos (fcd:get 'maxPanelWidth) 2 2)
      ", maxH="
      (rtos (fcd:get 'maxPanelHeight) 2 2)
      ", cutTouchTol="
      (rtos (fcd:get 'cutTouchTol) 2 2)
      ", prefix="
      (fcd:get 'markPrefix)
    )
  )

  (princ)
)


; -----------------------------
; Geometry creation
; -----------------------------

(defun fcd:make-rect-polyline (x1 y1 x2 y2 layerName / entData)
  (setq entData
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layerName)
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)
      (cons 10 (list x1 y1))
      (cons 10 (list x2 y1))
      (cons 10 (list x2 y2))
      (cons 10 (list x1 y2))
    )
  )

  (entmakex entData)
)


(defun fcd:make-cross-in-rect (x1 y1 x2 y2 layerName)
  (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      (cons 8 layerName)
      '(100 . "AcDbLine")
      (cons 10 (list x1 y1 0.0))
      (cons 11 (list x2 y2 0.0))
    )
  )

  (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      (cons 8 layerName)
      '(100 . "AcDbLine")
      (cons 10 (list x1 y2 0.0))
      (cons 11 (list x2 y1 0.0))
    )
  )
)


(defun fcd:make-text-centered (x y textValue height layerName)
  (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      (cons 8 layerName)
      '(100 . "AcDbText")
      (cons 10 (list x y 0.0))
      (cons 40 height)
      (cons 1 textValue)
      '(50 . 0.0)
      '(72 . 1)
      '(73 . 2)
      (cons 11 (list x y 0.0))
    )
  )
)


(defun fcd:label-in-rect (x1 y1 x2 y2 textValue layerName / w h textH cx cy)
  (setq w (abs (- x2 x1)))
  (setq h (abs (- y2 y1)))
  (setq cx (/ (+ x1 x2) 2.0))
  (setq cy (/ (+ y1 y2) 2.0))
  (setq textH (max 20.0 (min 100.0 (/ (min w h) 5.0))))
  (fcd:make-text-centered cx cy textValue textH layerName)
)


; -----------------------------
; Grid segment reader
; -----------------------------

(defun fcd:active-grid-layer ()
  (if (= (fcd:get 'mode) "AXIS")
    "FCD_GRID_AXIS"
    "FCD_GRID_EDGE"
  )
)


(defun fcd:grid-layer-p (layerName)
  (= (strcase layerName) (strcase (fcd:active-grid-layer)))
)

(defun fcd:line->segment (ent tol / obj type p1 p2 x1 y1 x2 y2 seg)
  (setq seg nil)
  (setq obj (vlax-ename->vla-object ent))
  (setq type (vla-get-ObjectName obj))

  (if (= type "AcDbLine")
    (progn
      (setq p1 (fcd:variant-point->list (vla-get-StartPoint obj)))
      (setq p2 (fcd:variant-point->list (vla-get-EndPoint obj)))

      (setq x1 (car p1))
      (setq y1 (cadr p1))
      (setq x2 (car p2))
      (setq y2 (cadr p2))

      (cond
        ((fcd:near= x1 x2 tol)
          ; vertical segment: (V x yMin yMax)
          (setq seg (list "V" x1 (min y1 y2) (max y1 y2)))
        )
        ((fcd:near= y1 y2 tol)
          ; horizontal segment: (H y xMin xMax)
          (setq seg (list "H" y1 (min x1 x2) (max x1 x2)))
        )
        (T
          (setq seg nil)
        )
      )
    )
  )

  seg
)


(defun fcd:collect-grid-segments (ss / i ent entData layerName seg xs ys vSegs hSegs ignored wrongLayer tol gridLayer)
  (setq xs nil)
  (setq ys nil)
  (setq vSegs nil)
  (setq hSegs nil)
  (setq ignored 0)
  (setq wrongLayer 0)
  (setq tol (fcd:get 'tol))
  (setq gridLayer (fcd:active-grid-layer))

  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq entData (entget ent))
    (setq layerName (cdr (assoc 8 entData)))

    ; Critical rule:
    ; Only the active grid layer is treated as grid.
    ; Openings, boundary, preview lines, and any accidental construction lines
    ; must not generate X/Y grid coordinates or small fake cells.
    (if (not (fcd:grid-layer-p layerName))
      (progn
        (setq ignored (1+ ignored))
        (setq wrongLayer (1+ wrongLayer))
      )
      (progn
        (setq seg (fcd:line->segment ent tol))

        (cond
          ((and seg (= (car seg) "V"))
            (setq xs (fcd:add-unique-number (cadr seg) xs tol))
            (setq vSegs
              (cons
                (list (cadr seg) (caddr seg) (cadddr seg))
                vSegs
              )
            )
          )
          ((and seg (= (car seg) "H"))
            (setq ys (fcd:add-unique-number (cadr seg) ys tol))
            (setq hSegs
              (cons
                (list (cadr seg) (caddr seg) (cadddr seg))
                hSegs
              )
            )
          )
          (T
            (setq ignored (1+ ignored))
          )
        )
      )
    )

    (setq i (1+ i))
  )

  (list
    (cons 'xs (fcd:sort-numbers xs))
    (cons 'ys (fcd:sort-numbers ys))
    (cons 'vSegs vSegs)
    (cons 'hSegs hSegs)
    (cons 'ignored ignored)
    (cons 'wrongLayer wrongLayer)
    (cons 'gridLayer gridLayer)
  )
)


; -----------------------------
; Segment coverage
; -----------------------------

(defun fcd:interval-covered-p (a b ranges tol / start end sorted covered r r1 r2 result)
  (setq start (min a b))
  (setq end (max a b))
  (setq result nil)

  (if (<= (- end start) tol)
    (setq result T)
    (progn
      (setq sorted
        (vl-sort ranges
          '(lambda (p q) (< (car p) (car q)))
        )
      )

      (setq covered start)

      (foreach r sorted
        (if (not result)
          (progn
            (setq r1 (car r))
            (setq r2 (cadr r))

            (if (<= r1 (+ covered tol))
              (progn
                (if (> r2 covered)
                  (setq covered r2)
                )

                (if (>= covered (- end tol))
                  (setq result T)
                )
              )
            )
          )
        )
      )
    )
  )

  result
)


(defun fcd:vertical-covered-p (x y1 y2 vSegs tol / ranges s)
  (setq ranges nil)

  (foreach s vSegs
    (if (fcd:near= (car s) x tol)
      (setq ranges
        (cons
          (list (cadr s) (caddr s))
          ranges
        )
      )
    )
  )

  (fcd:interval-covered-p y1 y2 ranges tol)
)


(defun fcd:horizontal-covered-p (y x1 x2 hSegs tol / ranges s)
  (setq ranges nil)

  (foreach s hSegs
    (if (fcd:near= (car s) y tol)
      (setq ranges
        (cons
          (list (cadr s) (caddr s))
          ranges
        )
      )
    )
  )

  (fcd:interval-covered-p x1 x2 ranges tol)
)


(defun fcd:cell-sides-exist-p (x1 y1 x2 y2 vSegs hSegs tol)
  (and
    (fcd:vertical-covered-p x1 y1 y2 vSegs tol)
    (fcd:vertical-covered-p x2 y1 y2 vSegs tol)
    (fcd:horizontal-covered-p y1 x1 x2 hSegs tol)
    (fcd:horizontal-covered-p y2 x1 x2 hSegs tol)
  )
)


(defun fcd:has-internal-divider-p (x1 y1 x2 y2 xs ys vSegs hSegs tol / divider x y)
  (setq divider nil)

  ; Full vertical line inside rectangle means this rectangle is actually divided.
  (foreach x xs
    (if
      (and
        (not divider)
        (fcd:strict-between x x1 x2 tol)
        (fcd:vertical-covered-p x y1 y2 vSegs tol)
      )
      (setq divider T)
    )
  )

  ; Full horizontal line inside rectangle means this rectangle is actually divided.
  (foreach y ys
    (if
      (and
        (not divider)
        (fcd:strict-between y y1 y2 tol)
        (fcd:horizontal-covered-p y x1 x2 hSegs tol)
      )
      (setq divider T)
    )
  )

  divider
)


; -----------------------------
; Real cell builder
; -----------------------------

(defun fcd:build-real-cells (xs ys vSegs hSegs / cells ix jx iy jy x1 x2 y1 y2 tol)
  (setq cells nil)
  (setq tol (fcd:get 'tol))

  (setq ix 0)
  (while (< ix (1- (length xs)))
    (setq jx (1+ ix))

    (while (< jx (length xs))
      (setq x1 (nth ix xs))
      (setq x2 (nth jx xs))

      (setq iy 0)
      (while (< iy (1- (length ys)))
        (setq jy (1+ iy))

        (while (< jy (length ys))
          (setq y1 (nth iy ys))
          (setq y2 (nth jy ys))

          (if
            (and
              (fcd:cell-sides-exist-p x1 y1 x2 y2 vSegs hSegs tol)
              (not (fcd:has-internal-divider-p x1 y1 x2 y2 xs ys vSegs hSegs tol))
            )
            (setq cells
              (cons
                (list x1 y1 x2 y2)
                cells
              )
            )
          )

          (setq jy (1+ jy))
        )

        (setq iy (1+ iy))
      )

      (setq jx (1+ jx))
    )

    (setq ix (1+ ix))
  )

  (reverse cells)
)


; -----------------------------
; Cell to panel rectangle
; -----------------------------

(defun fcd:cell->panel-rect (cell / x1 y1 x2 y2 mode joint half px1 py1 px2 py2)
  (setq x1 (nth 0 cell))
  (setq y1 (nth 1 cell))
  (setq x2 (nth 2 cell))
  (setq y2 (nth 3 cell))

  (setq mode (fcd:get 'mode))

  (if (= mode "AXIS")
    (progn
      (setq joint (fcd:get 'jointWidth))
      (setq half (/ joint 2.0))

      (setq px1 (+ x1 half))
      (setq py1 (+ y1 half))
      (setq px2 (- x2 half))
      (setq py2 (- y2 half))
    )
    (progn
      (setq px1 x1)
      (setq py1 y1)
      (setq px2 x2)
      (setq py2 y2)
    )
  )

  (if
    (and
      (> (- px2 px1) 0.0)
      (> (- py2 py1) 0.0)
    )
    (list px1 py1 px2 py2)
    nil
  )
)


; -----------------------------
; Boundary / openings geometry
; -----------------------------

(defun fcd:lwpoly->bbox-rect (ent / data pts xs ys)
  (setq data (entget ent))
  (setq pts
    (mapcar
      'cdr
      (vl-remove-if-not
        '(lambda (pair) (= (car pair) 10))
        data
      )
    )
  )

  (if pts
    (progn
      (setq xs (mapcar 'car pts))
      (setq ys (mapcar 'cadr pts))
      (list
        (apply 'min xs)
        (apply 'min ys)
        (apply 'max xs)
        (apply 'max ys)
      )
    )
    nil
  )
)


(defun fcd:add-unique-rect (rect rects tol / exists r)
  (setq exists nil)

  (foreach r rects
    (if (fcd:rect-equal-p r rect tol)
      (setq exists T)
    )
  )

  (if exists
    rects
    (cons rect rects)
  )
)

(defun fcd:collect-line-rects-on-layer (layerName / ss i ent seg xs ys vSegs hSegs cells rects c tol)
  (setq xs nil)
  (setq ys nil)
  (setq vSegs nil)
  (setq hSegs nil)
  (setq rects nil)
  (setq tol (fcd:get 'tol))

  (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 layerName))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq seg (fcd:line->segment ent tol))

        (cond
          ((and seg (= (car seg) "V"))
            (setq xs (fcd:add-unique-number (cadr seg) xs tol))
            (setq vSegs (cons (list (cadr seg) (caddr seg) (cadddr seg)) vSegs))
          )
          ((and seg (= (car seg) "H"))
            (setq ys (fcd:add-unique-number (cadr seg) ys tol))
            (setq hSegs (cons (list (cadr seg) (caddr seg) (cadddr seg)) hSegs))
          )
        )

        (setq i (1+ i))
      )

      (if (and (>= (length xs) 2) (>= (length ys) 2))
        (progn
          (setq cells (fcd:build-real-cells (fcd:sort-numbers xs) (fcd:sort-numbers ys) vSegs hSegs))
          (foreach c cells
            (setq rects (fcd:add-unique-rect c rects tol))
          )
        )
      )
    )
  )

  (reverse rects)
)

(defun fcd:collect-rects-on-layer (layerName / ss i ent rect rects lineRects tol)
  (setq rects nil)
  (setq tol (fcd:get 'tol))
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 layerName))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq rect (fcd:lwpoly->bbox-rect ent))

        (if rect
          (setq rects (fcd:add-unique-rect rect rects tol))
        )

        (setq i (1+ i))
      )
    )
  )

  ; Also supports rectangular openings/boundaries drawn as 4 LINE objects on the same layer.
  (setq lineRects (fcd:collect-line-rects-on-layer layerName))
  (foreach rect lineRects
    (setq rects (fcd:add-unique-rect rect rects tol))
  )

  (reverse rects)
)


(defun fcd:rect-contains-rect-p (outer inner tol)
  (and
    (>= (nth 0 inner) (- (nth 0 outer) tol))
    (>= (nth 1 inner) (- (nth 1 outer) tol))
    (<= (nth 2 inner) (+ (nth 2 outer) tol))
    (<= (nth 3 inner) (+ (nth 3 outer) tol))
  )
)


(defun fcd:rects-overlap-p (a b tol / ix1 iy1 ix2 iy2)
  (setq ix1 (max (nth 0 a) (nth 0 b)))
  (setq iy1 (max (nth 1 a) (nth 1 b)))
  (setq ix2 (min (nth 2 a) (nth 2 b)))
  (setq iy2 (min (nth 3 a) (nth 3 b)))

  (and
    (> (- ix2 ix1) tol)
    (> (- iy2 iy1) tol)
  )
)


(defun fcd:rect-inside-any-boundary-p (rect boundaries tol / ok b)
  ; If no boundary objects exist, boundary filtering is disabled.
  (setq ok nil)

  (if (not boundaries)
    T
    (progn
      (foreach b boundaries
        (if (and (not ok) (fcd:rect-contains-rect-p b rect tol))
          (setq ok T)
        )
      )
      ok
    )
  )
)


(defun fcd:rect-overlaps-any-opening-p (rect openings tol / hit o)
  (setq hit nil)

  (foreach o openings
    (if (and (not hit) (fcd:rects-overlap-p rect o tol))
      (setq hit T)
    )
  )

  hit
)


(defun fcd:scan-geometry (/ boundaries openings)
  (setq boundaries (fcd:collect-rects-on-layer "FCD_BOUNDARY"))
  (setq openings (fcd:collect-rects-on-layer "FCD_OPENINGS"))

  (fcd:msg
    (strcat
      "Геометрия: границ фасада на FCD_BOUNDARY: "
      (itoa (length boundaries))
      ", проемов на FCD_OPENINGS: "
      (itoa (length openings))
      "."
    )
  )

  (list
    (cons 'boundaries boundaries)
    (cons 'openings openings)
  )
)


(defun c:FCD_SCAN_GEOMETRY ()
  (fcd:init-layers)
  (fcd:scan-geometry)
  (princ)
)


; -----------------------------
; Opening cut classification
; -----------------------------

(defun fcd:bool-count (lst / n v)
  (setq n 0)
  (foreach v lst
    (if v (setq n (1+ n)))
  )
  n
)


(defun fcd:rect-intersection (a b tol / ix1 iy1 ix2 iy2)
  (setq ix1 (max (nth 0 a) (nth 0 b)))
  (setq iy1 (max (nth 1 a) (nth 1 b)))
  (setq ix2 (min (nth 2 a) (nth 2 b)))
  (setq iy2 (min (nth 3 a) (nth 3 b)))

  (if
    (and
      (> (- ix2 ix1) tol)
      (> (- iy2 iy1) tol)
    )
    (list ix1 iy1 ix2 iy2)
    nil
  )
)


(defun fcd:rect-equal-p (a b tol)
  (and
    (fcd:near= (nth 0 a) (nth 0 b) tol)
    (fcd:near= (nth 1 a) (nth 1 b) tol)
    (fcd:near= (nth 2 a) (nth 2 b) tol)
    (fcd:near= (nth 3 a) (nth 3 b) tol)
  )
)


(defun fcd:classify-single-cut (panel cut tol / pX1 pY1 pX2 pY2 cX1 cY1 cX2 cY2 touchTol touchL touchR touchB touchT nV nH shape corner side offset cutWidth cutDepth)
  (setq pX1 (nth 0 panel))
  (setq pY1 (nth 1 panel))
  (setq pX2 (nth 2 panel))
  (setq pY2 (nth 3 panel))

  (setq cX1 (nth 0 cut))
  (setq cY1 (nth 1 cut))
  (setq cX2 (nth 2 cut))
  (setq cY2 (nth 3 cut))

  ; Classification tolerance is intentionally larger than geometry tolerance.
  ; It prevents near-coincident opening edges from being classified as T-cuts instead of L-cuts.
  (setq touchTol (max tol (fcd:get 'cutTouchTol)))

  ; Snap cut rectangle to panel sides for classification/preview data.
  (if (<= (abs (- cX1 pX1)) touchTol) (setq cX1 pX1))
  (if (<= (abs (- cX2 pX2)) touchTol) (setq cX2 pX2))
  (if (<= (abs (- cY1 pY1)) touchTol) (setq cY1 pY1))
  (if (<= (abs (- cY2 pY2)) touchTol) (setq cY2 pY2))
  (setq cut (list cX1 cY1 cX2 cY2))

  (setq touchL (fcd:near= cX1 pX1 touchTol))
  (setq touchR (fcd:near= cX2 pX2 touchTol))
  (setq touchB (fcd:near= cY1 pY1 touchTol))
  (setq touchT (fcd:near= cY2 pY2 touchTol))

  (setq nV (fcd:bool-count (list touchL touchR)))
  (setq nH (fcd:bool-count (list touchB touchT)))

  (cond
    ; Opening covers the whole panel. This is not a panel; no block should be created later.
    ((fcd:rect-equal-p panel cut tol)
      (list
        (cons 'shape "OPENING_VOID")
        (cons 'cutRect cut)
      )
    )

    ; Corner cut: cut rectangle touches one vertical and one horizontal panel side.
    ((and (= nV 1) (= nH 1))
      (setq corner
        (cond
          ((and touchL touchT) "LEFT_TOP")
          ((and touchR touchT) "RIGHT_TOP")
          ((and touchL touchB) "LEFT_BOTTOM")
          ((and touchR touchB) "RIGHT_BOTTOM")
          (T "UNKNOWN")
        )
      )

      (list
        (cons 'shape "L_CUT")
        (cons 'cutRect cut)
        (cons 'cutCorner corner)
        (cons 'cutWidth (- cX2 cX1))
        (cons 'cutHeight (- cY2 cY1))
      )
    )

    ; Side cut from top/bottom, not from a corner.
    ((and (= nV 0) (= nH 1))
      (setq side (if touchT "TOP" "BOTTOM"))
      (setq offset (- cX1 pX1))
      (setq cutWidth (- cX2 cX1))
      (setq cutDepth
        (if touchT
          (- pY2 cY1)
          (- cY2 pY1)
        )
      )

      (list
        (cons 'shape "T_CUT")
        (cons 'cutRect cut)
        (cons 'cutSide side)
        (cons 'cutOffset offset)
        (cons 'cutWidth cutWidth)
        (cons 'cutDepth cutDepth)
      )
    )

    ; Side cut from left/right, not from a corner.
    ((and (= nV 1) (= nH 0))
      (setq side (if touchL "LEFT" "RIGHT"))
      (setq offset (- cY1 pY1))
      (setq cutWidth (- cY2 cY1))
      (setq cutDepth
        (if touchL
          (- cX2 pX1)
          (- pX2 cX1)
        )
      )

      (list
        (cons 'shape "T_CUT")
        (cons 'cutRect cut)
        (cons 'cutSide side)
        (cons 'cutOffset offset)
        (cons 'cutWidth cutWidth)
        (cons 'cutDepth cutDepth)
      )
    )

    ; Opening fully inside a panel or equivalent inner cut.
    ; Reserved for future U/hole-like implementation.
    ((and (= nV 0) (= nH 0))
      (list
        (cons 'shape "U_CUT_RESERVED")
        (cons 'cutRect cut)
      )
    )

    ; Opposite sides / strip / complex case.
    (T
      (list
        (cons 'shape "UNSUPPORTED")
        (cons 'cutRect cut)
      )
    )
  )
)


(defun fcd:classify-opening-hit (panel openings tol / hits o cut)
  (setq hits nil)

  (foreach o openings
    (setq cut (fcd:rect-intersection panel o tol))
    (if cut
      (setq hits (cons cut hits))
    )
  )

  (cond
    ((= (length hits) 0)
      (list (cons 'shape "RECT"))
    )

    ((> (length hits) 1)
      (list
        (cons 'shape "UNSUPPORTED")
        (cons 'reason "MULTIPLE_OPENINGS")
      )
    )

    (T
      (fcd:classify-single-cut panel (car hits) tol)
    )
  )
)


(defun fcd:preview-cut-panel (rect cutRect layerName label / x1 y1 x2 y2 cx1 cy1 cx2 cy2)
  (setq x1 (nth 0 rect))
  (setq y1 (nth 1 rect))
  (setq x2 (nth 2 rect))
  (setq y2 (nth 3 rect))

  (fcd:make-rect-polyline x1 y1 x2 y2 layerName)

  (if cutRect
    (progn
      (setq cx1 (nth 0 cutRect))
      (setq cy1 (nth 1 cutRect))
      (setq cx2 (nth 2 cutRect))
      (setq cy2 (nth 3 cutRect))
      (fcd:make-rect-polyline cx1 cy1 cx2 cy2 layerName)
    )
  )

  (fcd:label-in-rect x1 y1 x2 y2 label layerName)
)


; -----------------------------
; Preview writer
; -----------------------------

(defun fcd:preview-cells (cells boundaries openings / ok warnSmall errBig errBoundary openingVoid lCut tCut uReserved unsupported invalid cell rect cls shape cutRect x1 y1 x2 y2 w h minW minH maxW maxH tol)
  (setq ok 0)
  (setq warnSmall 0)
  (setq errBig 0)
  (setq errBoundary 0)
  (setq openingVoid 0)
  (setq lCut 0)
  (setq tCut 0)
  (setq uReserved 0)
  (setq unsupported 0)
  (setq invalid 0)

  (setq minW (fcd:get 'minPanelWidth))
  (setq minH (fcd:get 'minPanelHeight))
  (setq maxW (fcd:get 'maxPanelWidth))
  (setq maxH (fcd:get 'maxPanelHeight))
  (setq tol (fcd:get 'tol))

  (foreach cell cells
    (setq rect (fcd:cell->panel-rect cell))

    (if (not rect)
      (setq invalid (1+ invalid))
      (progn
        (setq x1 (nth 0 rect))
        (setq y1 (nth 1 rect))
        (setq x2 (nth 2 rect))
        (setq y2 (nth 3 rect))

        (setq w (- x2 x1))
        (setq h (- y2 y1))

        (cond
          ; Too small panel — warning.
          ((or (< w minW) (< h minH))
            (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_WARN")
            (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_WARN")
            (fcd:label-in-rect x1 y1 x2 y2 "SMALL" "FCD_PREVIEW_WARN")
            (setq warnSmall (1+ warnSmall))
          )

          ; Too large panel — whole panel is an error.
          ((or (> w maxW) (> h maxH))
            (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
            (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
            (fcd:label-in-rect x1 y1 x2 y2 "BIG" "FCD_PREVIEW_ERR")
            (setq errBig (1+ errBig))
          )

          ; Panel is outside facade boundary.
          ((not (fcd:rect-inside-any-boundary-p rect boundaries tol))
            (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
            (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
            (fcd:label-in-rect x1 y1 x2 y2 "OUT" "FCD_PREVIEW_ERR")
            (setq errBoundary (1+ errBoundary))
          )

          ; Opening processing: RECT / L_CUT / T_CUT / reserved / unsupported.
          (T
            (setq cls (fcd:classify-opening-hit rect openings tol))
            (setq shape (cdr (assoc 'shape cls)))
            (setq cutRect (cdr (assoc 'cutRect cls)))

            (cond
              ((= shape "RECT")
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_OK")
                (setq ok (1+ ok))
              )

              ((= shape "L_CUT")
                (fcd:preview-cut-panel rect cutRect "FCD_PREVIEW_LCUT" "L-CUT")
                (setq lCut (1+ lCut))
              )

              ((= shape "T_CUT")
                (fcd:preview-cut-panel rect cutRect "FCD_PREVIEW_TCUT" "T-CUT")
                (setq tCut (1+ tCut))
              )

              ((= shape "OPENING_VOID")
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_VOID")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_VOID")
                (fcd:label-in-rect x1 y1 x2 y2 "VOID" "FCD_PREVIEW_VOID")
                (setq openingVoid (1+ openingVoid))
              )

              ((= shape "U_CUT_RESERVED")
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (if cutRect
                  (fcd:make-rect-polyline (nth 0 cutRect) (nth 1 cutRect) (nth 2 cutRect) (nth 3 cutRect) "FCD_PREVIEW_ERR")
                )
                (fcd:label-in-rect x1 y1 x2 y2 "U-RES" "FCD_PREVIEW_ERR")
                (setq uReserved (1+ uReserved))
              )

              (T
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (if cutRect
                  (fcd:make-rect-polyline (nth 0 cutRect) (nth 1 cutRect) (nth 2 cutRect) (nth 3 cutRect) "FCD_PREVIEW_ERR")
                )
                (fcd:label-in-rect x1 y1 x2 y2 "UNSUP" "FCD_PREVIEW_ERR")
                (setq unsupported (1+ unsupported))
              )
            )
          )
        )
      )
    )
  )

  (list
    (cons 'ok ok)
    (cons 'warnSmall warnSmall)
    (cons 'errBig errBig)
    (cons 'errBoundary errBoundary)
    (cons 'openingVoid openingVoid)
    (cons 'lCut lCut)
    (cons 'tCut tCut)
    (cons 'uReserved uReserved)
    (cons 'unsupported unsupported)
    (cons 'invalid invalid)
  )
)


; -----------------------------
; Main preview command
; -----------------------------

(defun c:FCD_GRID_PREVIEW (/ ss geom boundaries openings grid xs ys vSegs hSegs ignored wrongLayer gridLayer cells stats ok warnSmall errBig errBoundary openingVoid lCut tCut uReserved unsupported invalid)

  (fcd:init-layers)
  (fcd:clear-preview)

  (setq geom (fcd:scan-geometry))
  (setq boundaries (cdr (assoc 'boundaries geom)))
  (setq openings (cdr (assoc 'openings geom)))

  (setq gridLayer (fcd:active-grid-layer))
  (fcd:msg
    (strcat
      "Выбери линии сетки на слое "
      gridLayer
      ". Окна/границы фасада считаются ограничениями автоматически и не должны быть линиями сетки."
    )
  )
  (setq ss (ssget (list (cons 0 "LINE") (cons 8 gridLayer))))

  (if (not ss)
    (progn
      (fcd:msg "Линии не выбраны.")
    )
    (progn
      (setq grid (fcd:collect-grid-segments ss))

      (setq xs (cdr (assoc 'xs grid)))
      (setq ys (cdr (assoc 'ys grid)))
      (setq vSegs (cdr (assoc 'vSegs grid)))
      (setq hSegs (cdr (assoc 'hSegs grid)))
      (setq ignored (cdr (assoc 'ignored grid)))
      (setq wrongLayer (cdr (assoc 'wrongLayer grid)))
      (setq gridLayer (cdr (assoc 'gridLayer grid)))

      (if (or (< (length xs) 2) (< (length ys) 2))
        (fcd:msg "Недостаточно линий. Нужно минимум 2 вертикальные и 2 горизонтальные.")
        (progn
          (setq cells (fcd:build-real-cells xs ys vSegs hSegs))
          (setq stats (fcd:preview-cells cells boundaries openings))

          (setq ok (cdr (assoc 'ok stats)))
          (setq warnSmall (cdr (assoc 'warnSmall stats)))
          (setq errBig (cdr (assoc 'errBig stats)))
          (setq errBoundary (cdr (assoc 'errBoundary stats)))
          (setq openingVoid (cdr (assoc 'openingVoid stats)))
          (setq lCut (cdr (assoc 'lCut stats)))
          (setq tCut (cdr (assoc 'tCut stats)))
          (setq uReserved (cdr (assoc 'uReserved stats)))
          (setq unsupported (cdr (assoc 'unsupported stats)))
          (setq invalid (cdr (assoc 'invalid stats)))

          (fcd:msg
            (strcat
              "Превью создано. "
              "слой сетки: " gridLayer
              ", X-линий: " (itoa (length xs))
              ", Y-линий: " (itoa (length ys))
              ", найдено реальных ячеек: " (itoa (length cells))
              ", OK: " (itoa ok)
              ", маленьких: " (itoa warnSmall)
              ", превышают максимум: " (itoa errBig)
              ", вне границы фасада: " (itoa errBoundary)
              ", L-вырезов: " (itoa lCut)
              ", T-вырезов: " (itoa tCut)
              ", внутри проемов: " (itoa openingVoid)
              ", П/резерв: " (itoa uReserved)
              ", неподдерживаемых: " (itoa unsupported)
              ", невалидных: " (itoa invalid)
              ", проигнорировано линий: " (itoa ignored)
              ", не на слое сетки: " (itoa wrongLayer)
              "."
            )
          )
        )
      )
    )
  )

  (princ)
)






; ============================================================
; V08 OVERRIDES
;
; User decision:
; - All selected geometry from all drawing layers can participate in the grid.
; - Boundary/opening contours are not excluded from grid anymore.
; - Preview/service result layers are still skipped to avoid self-feedback.
; - LINE and orthogonal LWPOLYLINE are supported as grid segment sources.
; - Openings are collected as actual polygon/rectangle constraints.
; ============================================================

(defun fcd:service-preview-layer-p (layerName / u)
  (setq u (strcase layerName))
  (or
    (wcmatch u "FCD_PREVIEW_*")
    (= u "FCD_PANELS")
    (= u "FCD_TRIMS")
  )
)

(defun fcd:active-grid-layer ()
  "ALL_SELECTED_LAYERS"
)

(defun fcd:grid-layer-p (layerName)
  (not (fcd:service-preview-layer-p layerName))
)

(defun fcd:lwpoly->vertices (ent / data pts)
  (setq data (entget ent))
  (setq pts
    (mapcar
      'cdr
      (vl-remove-if-not
        '(lambda (pair) (= (car pair) 10))
        data
      )
    )
  )
  pts
)

(defun fcd:lwpoly-closed-p (ent / data flag)
  (setq data (entget ent))
  (setq flag (cdr (assoc 70 data)))
  (if flag
    (= 1 (logand 1 flag))
    nil
  )
)

(defun fcd:bbox-from-pts (pts / xs ys)
  (if pts
    (progn
      (setq xs (mapcar 'car pts))
      (setq ys (mapcar 'cadr pts))
      (list
        (apply 'min xs)
        (apply 'min ys)
        (apply 'max xs)
        (apply 'max ys)
      )
    )
    nil
  )
)

(defun fcd:segment-from-points (p1 p2 tol / x1 y1 x2 y2 seg)
  (setq seg nil)
  (setq x1 (car p1))
  (setq y1 (cadr p1))
  (setq x2 (car p2))
  (setq y2 (cadr p2))

  (cond
    ((and (fcd:near= x1 x2 tol) (> (abs (- y2 y1)) tol))
      (setq seg (list "V" x1 (min y1 y2) (max y1 y2)))
    )
    ((and (fcd:near= y1 y2 tol) (> (abs (- x2 x1)) tol))
      (setq seg (list "H" y1 (min x1 x2) (max x1 x2)))
    )
    (T
      (setq seg nil)
    )
  )

  seg
)

(defun fcd:entity->grid-segments (ent tol / obj type p1 p2 pts closed n i pA pB seg segs)
  (setq segs nil)
  (setq obj (vlax-ename->vla-object ent))
  (setq type (vla-get-ObjectName obj))

  (cond
    ((= type "AcDbLine")
      (setq seg (fcd:line->segment ent tol))
      (if seg (setq segs (list seg)))
    )

    ((= type "AcDbPolyline")
      (setq pts (fcd:lwpoly->vertices ent))
      (setq closed (fcd:lwpoly-closed-p ent))
      (setq n (length pts))

      (if (>= n 2)
        (progn
          (setq i 0)
          (while (< i (1- n))
            (setq pA (nth i pts))
            (setq pB (nth (1+ i) pts))
            (setq seg (fcd:segment-from-points pA pB tol))
            (if seg (setq segs (cons seg segs)))
            (setq i (1+ i))
          )

          (if closed
            (progn
              (setq pA (nth (1- n) pts))
              (setq pB (nth 0 pts))
              (setq seg (fcd:segment-from-points pA pB tol))
              (if seg (setq segs (cons seg segs)))
            )
          )
        )
      )
    )
  )

  (reverse segs)
)

(defun fcd:collect-grid-segments (ss / i ent entData layerName seg segs xs ys vSegs hSegs ignored skippedService tol)
  (setq xs nil)
  (setq ys nil)
  (setq vSegs nil)
  (setq hSegs nil)
  (setq ignored 0)
  (setq skippedService 0)
  (setq tol (fcd:get 'tol))

  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq entData (entget ent))
    (setq layerName (cdr (assoc 8 entData)))

    (if (fcd:service-preview-layer-p layerName)
      (progn
        (setq ignored (1+ ignored))
        (setq skippedService (1+ skippedService))
      )
      (progn
        (setq segs (fcd:entity->grid-segments ent tol))
        (if (not segs)
          (setq ignored (1+ ignored))
          (foreach seg segs
            (cond
              ((= (car seg) "V")
                (setq xs (fcd:add-unique-number (cadr seg) xs tol))
                (setq vSegs
                  (cons
                    (list (cadr seg) (caddr seg) (cadddr seg))
                    vSegs
                  )
                )
              )
              ((= (car seg) "H")
                (setq ys (fcd:add-unique-number (cadr seg) ys tol))
                (setq hSegs
                  (cons
                    (list (cadr seg) (caddr seg) (cadddr seg))
                    hSegs
                  )
                )
              )
              (T
                (setq ignored (1+ ignored))
              )
            )
          )
        )
      )
    )

    (setq i (1+ i))
  )

  (list
    (cons 'xs (fcd:sort-numbers xs))
    (cons 'ys (fcd:sort-numbers ys))
    (cons 'vSegs vSegs)
    (cons 'hSegs hSegs)
    (cons 'ignored ignored)
    (cons 'wrongLayer skippedService)
    (cons 'gridLayer "ALL_SELECTED_LAYERS")
  )
)

; -----------------------------
; Shape geometry helpers
; -----------------------------

(defun fcd:shape-type (shape)
  (cdr (assoc 'type shape))
)

(defun fcd:shape-bbox (shape)
  (cdr (assoc 'bbox shape))
)

(defun fcd:shape-rect (shape)
  (cdr (assoc 'rect shape))
)

(defun fcd:shape-pts (shape)
  (cdr (assoc 'pts shape))
)

(defun fcd:make-rect-shape (rect)
  (list
    (cons 'type "RECT")
    (cons 'rect rect)
    (cons 'bbox rect)
  )
)

(defun fcd:point-in-rect-p (pt rect tol)
  (and
    (>= (car pt) (- (nth 0 rect) tol))
    (>= (cadr pt) (- (nth 1 rect) tol))
    (<= (car pt) (+ (nth 2 rect) tol))
    (<= (cadr pt) (+ (nth 3 rect) tol))
  )
)

(defun fcd:rect-corners (rect)
  (list
    (list (nth 0 rect) (nth 1 rect))
    (list (nth 2 rect) (nth 1 rect))
    (list (nth 2 rect) (nth 3 rect))
    (list (nth 0 rect) (nth 3 rect))
  )
)

(defun fcd:all-points-in-rect-corners-p (pts bbox tol / ok p)
  (setq ok T)
  (foreach p pts
    (if
      (not
        (and
          (or (fcd:near= (car p) (nth 0 bbox) tol) (fcd:near= (car p) (nth 2 bbox) tol))
          (or (fcd:near= (cadr p) (nth 1 bbox) tol) (fcd:near= (cadr p) (nth 3 bbox) tol))
        )
      )
      (setq ok nil)
    )
  )
  ok
)

(defun fcd:rectangular-poly-p (pts tol / bbox w h)
  (setq bbox (fcd:bbox-from-pts pts))
  (if (not bbox)
    nil
    (progn
      (setq w (- (nth 2 bbox) (nth 0 bbox)))
      (setq h (- (nth 3 bbox) (nth 1 bbox)))
      (and
        (> w tol)
        (> h tol)
        (>= (length pts) 4)
        (fcd:all-points-in-rect-corners-p pts bbox tol)
      )
    )
  )
)

(defun fcd:make-poly-shape (pts / bbox tol)
  (setq tol (fcd:get 'tol))
  (setq bbox (fcd:bbox-from-pts pts))

  (if (not bbox)
    nil
    (if (fcd:rectangular-poly-p pts tol)
      (fcd:make-rect-shape bbox)
      (list
        (cons 'type "POLY")
        (cons 'pts pts)
        (cons 'bbox bbox)
      )
    )
  )
)

(defun fcd:point-on-segment-p (pt a b tol / x y x1 y1 x2 y2 cross dot len2)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq x1 (car a))
  (setq y1 (cadr a))
  (setq x2 (car b))
  (setq y2 (cadr b))

  (setq cross (- (* (- x x1) (- y2 y1)) (* (- y y1) (- x2 x1))))

  (if (> (abs cross) tol)
    nil
    (progn
      (setq dot (+ (* (- x x1) (- x2 x1)) (* (- y y1) (- y2 y1))))
      (setq len2 (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))
      (and
        (>= dot (- tol))
        (<= dot (+ len2 tol))
      )
    )
  )
)

(defun fcd:point-on-poly-boundary-p (pt pts tol / hit n i a b)
  (setq hit nil)
  (setq n (length pts))
  (if (>= n 2)
    (progn
      (setq i 0)
      (while (< i n)
        (setq a (nth i pts))
        (setq b (nth (rem (1+ i) n) pts))
        (if (and (not hit) (fcd:point-on-segment-p pt a b tol))
          (setq hit T)
        )
        (setq i (1+ i))
      )
    )
  )
  hit
)

(defun fcd:point-in-poly-p (pt pts tol / x y inside n i j pi pj xi yi xj yj xCross)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq inside nil)
  (setq n (length pts))

  (if (fcd:point-on-poly-boundary-p pt pts tol)
    T
    (progn
      (setq i 0)
      (setq j (1- n))
      (while (< i n)
        (setq pi (nth i pts))
        (setq pj (nth j pts))
        (setq xi (car pi))
        (setq yi (cadr pi))
        (setq xj (car pj))
        (setq yj (cadr pj))

        (if
          (and
            (/= (> yi y) (> yj y))
            (/= yi yj)
          )
          (progn
            (setq xCross (+ xi (/ (* (- y yi) (- xj xi)) (- yj yi))))
            (if (< x xCross)
              (setq inside (not inside))
            )
          )
        )

        (setq j i)
        (setq i (1+ i))
      )
      inside
    )
  )
)

(defun fcd:cross2 (a b c)
  (-
    (* (- (car b) (car a)) (- (cadr c) (cadr a)))
    (* (- (cadr b) (cadr a)) (- (car c) (car a)))
  )
)

(defun fcd:segments-intersect-p (a b c d tol / c1 c2 c3 c4)
  (setq c1 (fcd:cross2 a b c))
  (setq c2 (fcd:cross2 a b d))
  (setq c3 (fcd:cross2 c d a))
  (setq c4 (fcd:cross2 c d b))

  (or
    (and (<= (* c1 c2) tol) (<= (* c3 c4) tol)
         (not
           (or
             (< (max (car a) (car b)) (- (min (car c) (car d)) tol))
             (< (max (car c) (car d)) (- (min (car a) (car b)) tol))
             (< (max (cadr a) (cadr b)) (- (min (cadr c) (cadr d)) tol))
             (< (max (cadr c) (cadr d)) (- (min (cadr a) (cadr b)) tol))
           )
         )
    )
    (fcd:point-on-segment-p a c d tol)
    (fcd:point-on-segment-p b c d tol)
    (fcd:point-on-segment-p c a b tol)
    (fcd:point-on-segment-p d a b tol)
  )
)

(defun fcd:rect-edges (rect / c)
  (setq c (fcd:rect-corners rect))
  (list
    (list (nth 0 c) (nth 1 c))
    (list (nth 1 c) (nth 2 c))
    (list (nth 2 c) (nth 3 c))
    (list (nth 3 c) (nth 0 c))
  )
)

(defun fcd:poly-edges (pts / edges n i)
  (setq edges nil)
  (setq n (length pts))
  (if (>= n 2)
    (progn
      (setq i 0)
      (while (< i n)
        (setq edges (cons (list (nth i pts) (nth (rem (1+ i) n) pts)) edges))
        (setq i (1+ i))
      )
    )
  )
  edges
)

(defun fcd:shape-contains-rect-p (shape rect tol / typ pts corners ok p)
  (setq typ (fcd:shape-type shape))
  (cond
    ((= typ "RECT")
      (fcd:rect-contains-rect-p (fcd:shape-rect shape) rect tol)
    )
    ((= typ "POLY")
      (setq pts (fcd:shape-pts shape))
      (setq corners (fcd:rect-corners rect))
      (setq ok T)
      (foreach p corners
        (if (not (fcd:point-in-poly-p p pts tol))
          (setq ok nil)
        )
      )
      ok
    )
    (T nil)
  )
)

(defun fcd:rect-overlaps-poly-p (rect pts tol / hit p e1 e2 rectEdges polyEdges)
  (setq hit nil)

  ; Any rectangle corner inside polygon.
  (foreach p (fcd:rect-corners rect)
    (if (and (not hit) (fcd:point-in-poly-p p pts tol))
      (setq hit T)
    )
  )

  ; Any polygon vertex inside rectangle.
  (foreach p pts
    (if (and (not hit) (fcd:point-in-rect-p p rect tol))
      (setq hit T)
    )
  )

  ; Any edge intersection.
  (if (not hit)
    (progn
      (setq rectEdges (fcd:rect-edges rect))
      (setq polyEdges (fcd:poly-edges pts))
      (foreach e1 rectEdges
        (foreach e2 polyEdges
          (if
            (and
              (not hit)
              (fcd:segments-intersect-p (car e1) (cadr e1) (car e2) (cadr e2) tol)
            )
            (setq hit T)
          )
        )
      )
    )
  )

  hit
)

(defun fcd:shape-overlaps-rect-p (shape rect tol / typ)
  (setq typ (fcd:shape-type shape))
  (cond
    ((= typ "RECT")
      (fcd:rects-overlap-p rect (fcd:shape-rect shape) tol)
    )
    ((= typ "POLY")
      (fcd:rect-overlaps-poly-p rect (fcd:shape-pts shape) tol)
    )
    (T nil)
  )
)

(defun fcd:collect-shapes-on-layer (layerName / shapes ss i ent pts shape lineRects rect tol)
  (setq shapes nil)
  (setq tol (fcd:get 'tol))

  ; Closed polylines: rectangle if rectangular, polygon otherwise.
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 layerName))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq pts (fcd:lwpoly->vertices ent))
        (if pts
          (progn
            (setq shape (fcd:make-poly-shape pts))
            (if shape (setq shapes (cons shape shapes)))
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ; Rectangles drawn as 4 LINE objects on the layer.
  (setq lineRects (fcd:collect-line-rects-on-layer layerName))
  (foreach rect lineRects
    (setq shapes (cons (fcd:make-rect-shape rect) shapes))
  )

  (reverse shapes)
)

(defun fcd:rect-inside-any-boundary-p (rect boundaries tol / ok b)
  ; If no boundary exists, boundary filtering is disabled.
  (if (not boundaries)
    T
    (progn
      (setq ok nil)
      (foreach b boundaries
        (if (and (not ok) (fcd:shape-contains-rect-p b rect tol))
          (setq ok T)
        )
      )
      ok
    )
  )
)

(defun fcd:scan-geometry (/ boundaries openings)
  (setq boundaries (fcd:collect-shapes-on-layer "FCD_BOUNDARY"))
  (setq openings (fcd:collect-shapes-on-layer "FCD_OPENINGS"))

  (fcd:msg
    (strcat
      "Геометрия: границ фасада на FCD_BOUNDARY: "
      (itoa (length boundaries))
      ", проемов на FCD_OPENINGS: "
      (itoa (length openings))
      "."
    )
  )

  (list
    (cons 'boundaries boundaries)
    (cons 'openings openings)
  )
)

(defun fcd:classify-opening-hit (panel openings tol / rectHits polyPartial o typ cut voidHit)
  (setq rectHits nil)
  (setq polyPartial nil)
  (setq voidHit nil)

  (foreach o openings
    (setq typ (fcd:shape-type o))

    (cond
      ; Any opening shape fully contains this cell: it is an opening void, not a panel.
      ((fcd:shape-contains-rect-p o panel tol)
        (setq voidHit T)
      )

      ; Rectangular opening: preserve L/T classification.
      ((= typ "RECT")
        (setq cut (fcd:rect-intersection panel (fcd:shape-rect o) tol))
        (if cut (setq rectHits (cons cut rectHits)))
      )

      ; Complex opening: partial overlap exists, but not reducible to current L/T rectangular block.
      ((and (= typ "POLY") (fcd:shape-overlaps-rect-p o panel tol))
        (setq polyPartial T)
      )
    )
  )

  (cond
    ((and voidHit (not rectHits) (not polyPartial))
      (list (cons 'shape "OPENING_VOID"))
    )

    ((or polyPartial (> (length rectHits) 1) (and voidHit rectHits))
      (list
        (cons 'shape "UNSUPPORTED")
        (cons 'reason "COMPLEX_OPENING")
      )
    )

    ((= (length rectHits) 0)
      (list (cons 'shape "RECT"))
    )

    (T
      (fcd:classify-single-cut panel (car rectHits) tol)
    )
  )
)

(defun fcd:preview-cells (cells boundaries openings / ok warnSmall errBig errBoundary openingVoid lCut tCut uReserved unsupported invalid cell rect cls shape cutRect x1 y1 x2 y2 w h minW minH maxW maxH tol)
  (setq ok 0)
  (setq warnSmall 0)
  (setq errBig 0)
  (setq errBoundary 0)
  (setq openingVoid 0)
  (setq lCut 0)
  (setq tCut 0)
  (setq uReserved 0)
  (setq unsupported 0)
  (setq invalid 0)

  (setq minW (fcd:get 'minPanelWidth))
  (setq minH (fcd:get 'minPanelHeight))
  (setq maxW (fcd:get 'maxPanelWidth))
  (setq maxH (fcd:get 'maxPanelHeight))
  (setq tol (fcd:get 'tol))

  (foreach cell cells
    (setq rect (fcd:cell->panel-rect cell))

    (if (not rect)
      (setq invalid (1+ invalid))
      (progn
        (setq x1 (nth 0 rect))
        (setq y1 (nth 1 rect))
        (setq x2 (nth 2 rect))
        (setq y2 (nth 3 rect))
        (setq w (- x2 x1))
        (setq h (- y2 y1))

        (cond
          ; Boundary check must happen before size check.
          ((not (fcd:rect-inside-any-boundary-p rect boundaries tol))
            (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
            (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
            (fcd:label-in-rect x1 y1 x2 y2 "OUT" "FCD_PREVIEW_ERR")
            (setq errBoundary (1+ errBoundary))
          )

          (T
            ; Opening classification before size check: small cells inside openings should be VOID, not SMALL.
            (setq cls (fcd:classify-opening-hit rect openings tol))
            (setq shape (cdr (assoc 'shape cls)))
            (setq cutRect (cdr (assoc 'cutRect cls)))

            (cond
              ((= shape "OPENING_VOID")
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_VOID")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_VOID")
                (fcd:label-in-rect x1 y1 x2 y2 "VOID" "FCD_PREVIEW_VOID")
                (setq openingVoid (1+ openingVoid))
              )

              ((or (< w minW) (< h minH))
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_WARN")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_WARN")
                (fcd:label-in-rect x1 y1 x2 y2 "SMALL" "FCD_PREVIEW_WARN")
                (setq warnSmall (1+ warnSmall))
              )

              ((or (> w maxW) (> h maxH))
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (fcd:label-in-rect x1 y1 x2 y2 "BIG" "FCD_PREVIEW_ERR")
                (setq errBig (1+ errBig))
              )

              ((= shape "RECT")
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_OK")
                (setq ok (1+ ok))
              )

              ((= shape "L_CUT")
                (fcd:preview-cut-panel rect cutRect "FCD_PREVIEW_LCUT" "L-CUT")
                (setq lCut (1+ lCut))
              )

              ((= shape "T_CUT")
                (fcd:preview-cut-panel rect cutRect "FCD_PREVIEW_TCUT" "T-CUT")
                (setq tCut (1+ tCut))
              )

              ((= shape "U_CUT_RESERVED")
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (if cutRect
                  (fcd:make-rect-polyline (nth 0 cutRect) (nth 1 cutRect) (nth 2 cutRect) (nth 3 cutRect) "FCD_PREVIEW_ERR")
                )
                (fcd:label-in-rect x1 y1 x2 y2 "U-RES" "FCD_PREVIEW_ERR")
                (setq uReserved (1+ uReserved))
              )

              (T
                (fcd:make-rect-polyline x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
                (if cutRect
                  (fcd:make-rect-polyline (nth 0 cutRect) (nth 1 cutRect) (nth 2 cutRect) (nth 3 cutRect) "FCD_PREVIEW_ERR")
                )
                (fcd:label-in-rect x1 y1 x2 y2 "UNSUP" "FCD_PREVIEW_ERR")
                (setq unsupported (1+ unsupported))
              )
            )
          )
        )
      )
    )
  )

  (list
    (cons 'ok ok)
    (cons 'warnSmall warnSmall)
    (cons 'errBig errBig)
    (cons 'errBoundary errBoundary)
    (cons 'openingVoid openingVoid)
    (cons 'lCut lCut)
    (cons 'tCut tCut)
    (cons 'uReserved uReserved)
    (cons 'unsupported unsupported)
    (cons 'invalid invalid)
  )
)

(defun c:FCD_GRID_PREVIEW (/ ss geom boundaries openings grid xs ys vSegs hSegs ignored wrongLayer gridLayer cells stats ok warnSmall errBig errBoundary openingVoid lCut tCut uReserved unsupported invalid)

  (fcd:init-layers)
  (fcd:clear-preview)

  (setq geom (fcd:scan-geometry))
  (setq boundaries (cdr (assoc 'boundaries geom)))
  (setq openings (cdr (assoc 'openings geom)))

  (fcd:msg
    "Выбери всю геометрию раскладки: сетку, внешний контур, проемы. LINE и ортогональные LWPOLYLINE с любых слоев будут считаться сеткой. FCD_PREVIEW_* будет пропущен."
  )
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

  (if (not ss)
    (progn
      (fcd:msg "Геометрия не выбрана.")
    )
    (progn
      (setq grid (fcd:collect-grid-segments ss))

      (setq xs (cdr (assoc 'xs grid)))
      (setq ys (cdr (assoc 'ys grid)))
      (setq vSegs (cdr (assoc 'vSegs grid)))
      (setq hSegs (cdr (assoc 'hSegs grid)))
      (setq ignored (cdr (assoc 'ignored grid)))
      (setq wrongLayer (cdr (assoc 'wrongLayer grid)))
      (setq gridLayer (cdr (assoc 'gridLayer grid)))

      (if (or (< (length xs) 2) (< (length ys) 2))
        (fcd:msg "Недостаточно геометрии. Нужно минимум 2 вертикальные и 2 горизонтальные линии/сегмента.")
        (progn
          (setq cells (fcd:build-real-cells xs ys vSegs hSegs))
          (setq stats (fcd:preview-cells cells boundaries openings))

          (setq ok (cdr (assoc 'ok stats)))
          (setq warnSmall (cdr (assoc 'warnSmall stats)))
          (setq errBig (cdr (assoc 'errBig stats)))
          (setq errBoundary (cdr (assoc 'errBoundary stats)))
          (setq openingVoid (cdr (assoc 'openingVoid stats)))
          (setq lCut (cdr (assoc 'lCut stats)))
          (setq tCut (cdr (assoc 'tCut stats)))
          (setq uReserved (cdr (assoc 'uReserved stats)))
          (setq unsupported (cdr (assoc 'unsupported stats)))
          (setq invalid (cdr (assoc 'invalid stats)))

          (fcd:msg
            (strcat
              "Превью создано. "
              "источник сетки: " gridLayer
              ", X-координат: " (itoa (length xs))
              ", Y-координат: " (itoa (length ys))
              ", найдено реальных ячеек: " (itoa (length cells))
              ", OK: " (itoa ok)
              ", маленьких: " (itoa warnSmall)
              ", превышают максимум: " (itoa errBig)
              ", вне границы фасада: " (itoa errBoundary)
              ", L-вырезов: " (itoa lCut)
              ", T-вырезов: " (itoa tCut)
              ", внутри проемов: " (itoa openingVoid)
              ", П/резерв: " (itoa uReserved)
              ", неподдерживаемых: " (itoa unsupported)
              ", невалидных: " (itoa invalid)
              ", проигнорировано сегментов/объектов: " (itoa ignored)
              ", пропущено служебных preview-объектов: " (itoa wrongLayer)
              "."
            )
          )
        )
      )
    )
  )

  (princ)
)

(princ "\nFCD_CORE_V08 loaded. Commands: FCD_SETUP, FCD_INITLAYERS, FCD_CLEAR_PREVIEW, FCD_SCAN_GEOMETRY, FCD_GRID_PREVIEW")
(princ)



; ============================================================
; V10 OVERRIDES
;
; Decision after tests:
; - Region flood approach was too aggressive and generated extra contours.
; - Return to reliable real-rectangle grid detection from V08.
; - Add explicit closed non-rectangular LWPOLYLINE detection as first-class panels.
; - Closed complex outlines are previewed as one outline, not as many atomic rectangles.
; - Rectangular grid cells whose center is inside explicit complex panel outlines are skipped.
; ============================================================

(defun fcd:layer-name-of (ent / d)
  (setq d (entget ent))
  (cdr (assoc 8 d))
)

(defun fcd:explicit-shape-layer-p (layerName / u)
  (setq u (strcase layerName))
  (and
    (not (fcd:service-preview-layer-p layerName))
    (/= u "FCD_BOUNDARY")
    (/= u "FCD_OPENINGS")
  )
)

(defun fcd:make-poly-preview (pts layerName / data p)
  (if (and pts (>= (length pts) 3))
    (progn
      (setq data
        (list
          '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          (cons 8 layerName)
          '(100 . "AcDbPolyline")
          (cons 90 (length pts))
          '(70 . 1)
        )
      )
      (foreach p pts
        (setq data (append data (list (cons 10 (list (car p) (cadr p))))))
      )
      (entmakex data)
    )
  )
)

(defun fcd:polyline-orthogonal-p (pts tol / ok n i a b)
  (setq ok T)
  (setq n (length pts))
  (if (< n 3)
    nil
    (progn
      (setq i 0)
      (while (< i n)
        (setq a (nth i pts))
        (setq b (nth (rem (1+ i) n) pts))
        (if
          (not
            (or
              (fcd:near= (car a) (car b) tol)
              (fcd:near= (cadr a) (cadr b) tol)
            )
          )
          (setq ok nil)
        )
        (setq i (1+ i))
      )
      ok
    )
  )
)

(defun fcd:add-unique-int-pair (p lst / hit x)
  (setq hit nil)
  (foreach x lst
    (if (and (= (car x) (car p)) (= (cadr x) (cadr p)))
      (setq hit T)
    )
  )
  (if hit lst (cons p lst))
)

(defun fcd:index-point-present-p (i j pts / hit p)
  (setq hit nil)
  (foreach p pts
    (if (and (not hit) (= i (car p)) (= j (cadr p)))
      (setq hit T)
    )
  )
  hit
)

(defun fcd:index-neighbours (pt pts / i j candidates out p)
  (setq i (car pt))
  (setq j (cadr pt))
  (setq candidates
    (list
      (list (1- i) j)
      (list (1+ i) j)
      (list i (1- j))
      (list i (1+ j))
    )
  )
  (setq out nil)
  (foreach p candidates
    (if (fcd:index-point-present-p (car p) (cadr p) pts)
      (setq out (cons p out))
    )
  )
  out
)

(defun fcd:index-flood (start pts / queue visited p ns n)
  (setq queue (list start))
  (setq visited nil)
  (while queue
    (setq p (car queue))
    (setq queue (cdr queue))
    (if (not (member p visited))
      (progn
        (setq visited (cons p visited))
        (setq ns (fcd:index-neighbours p pts))
        (foreach n ns
          (if (not (member n visited))
            (setq queue (cons n queue))
          )
        )
      )
    )
  )
  visited
)

(defun fcd:index-components (pts / unvisited comps start comp p)
  (setq unvisited pts)
  (setq comps nil)
  (while unvisited
    (setq start (car unvisited))
    (setq comp (fcd:index-flood start pts))
    (setq comps (cons comp comps))
    (foreach p comp
      (setq unvisited (vl-remove p unvisited))
    )
  )
  (reverse comps)
)

(defun fcd:remove-duplicates-equal (lst / out x)
  (setq out nil)
  (foreach x lst
    (if (not (member x out))
      (setq out (cons x out))
    )
  )
  (reverse out)
)

(defun fcd:component-touches-sides (comp minI minJ maxI maxJ / touches p)
  (setq touches nil)
  (foreach p comp
    (if (= (car p) minI) (setq touches (cons "LEFT" touches)))
    (if (= (car p) maxI) (setq touches (cons "RIGHT" touches)))
    (if (= (cadr p) minJ) (setq touches (cons "BOTTOM" touches)))
    (if (= (cadr p) maxJ) (setq touches (cons "TOP" touches)))
  )
  (fcd:remove-duplicates-equal touches)
)

(defun fcd:all-components-touch-common-side-p (components minI minJ maxI maxJ / common comp sides s newCommon)
  (setq common (list "LEFT" "RIGHT" "BOTTOM" "TOP"))
  (foreach comp components
    (setq sides (fcd:component-touches-sides comp minI minJ maxI maxJ))
    (setq newCommon nil)
    (foreach s common
      (if (member s sides)
        (setq newCommon (cons s newCommon))
      )
    )
    (setq common newCommon)
  )
  (if common T nil)
)

(defun fcd:adjacent-sides-p (sides)
  (or
    (and (member "LEFT" sides) (member "TOP" sides))
    (and (member "LEFT" sides) (member "BOTTOM" sides))
    (and (member "RIGHT" sides) (member "TOP" sides))
    (and (member "RIGHT" sides) (member "BOTTOM" sides))
  )
)

(defun fcd:opposite-sides-p (sides)
  (or
    (and (member "LEFT" sides) (member "RIGHT" sides))
    (and (member "TOP" sides) (member "BOTTOM" sides))
  )
)

(defun fcd:union-side-list (components minI minJ maxI maxJ / out comp s)
  (setq out nil)
  (foreach comp components
    (foreach s (fcd:component-touches-sides comp minI minJ maxI maxJ)
      (if (not (member s out))
        (setq out (cons s out))
      )
    )
  )
  (fcd:remove-duplicates-equal out)
)

(defun fcd:classify-orthogonal-poly-shape (pts / xs ys minI minJ maxI maxJ filled missing i j x1 x2 y1 y2 cx cy components sideUnion firstSides tol)
  (setq tol (fcd:get 'tol))
  (setq xs nil)
  (setq ys nil)

  (foreach p pts
    (setq xs (fcd:add-unique-number (car p) xs tol))
    (setq ys (fcd:add-unique-number (cadr p) ys tol))
  )

  (setq xs (fcd:sort-numbers xs))
  (setq ys (fcd:sort-numbers ys))

  (if (or (< (length xs) 2) (< (length ys) 2))
    "UNSUPPORTED"
    (progn
      (setq minI 0)
      (setq minJ 0)
      (setq maxI (- (length xs) 2))
      (setq maxJ (- (length ys) 2))
      (setq filled nil)
      (setq missing nil)

      (setq i 0)
      (while (< i (1- (length xs)))
        (setq x1 (nth i xs))
        (setq x2 (nth (1+ i) xs))
        (setq j 0)
        (while (< j (1- (length ys)))
          (setq y1 (nth j ys))
          (setq y2 (nth (1+ j) ys))
          (setq cx (/ (+ x1 x2) 2.0))
          (setq cy (/ (+ y1 y2) 2.0))
          (if (fcd:point-in-poly-p (list cx cy) pts tol)
            (setq filled (fcd:add-unique-int-pair (list i j) filled))
            (setq missing (fcd:add-unique-int-pair (list i j) missing))
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )

      (if (not missing)
        "RECT"
        (progn
          (setq components (fcd:index-components missing))
          (setq sideUnion (fcd:union-side-list components minI minJ maxI maxJ))

          (cond
            ; bbox minus one corner zone: classic L / Г shape.
            ((and (= (length components) 1)
                  (= (length sideUnion) 2)
                  (fcd:adjacent-sides-p sideUnion))
              "L_CUT"
            )

            ; T-shaped panel: two missing side zones with one common side.
            ; Example: top bar + central stem, or any rotated equivalent.
            ((and (= (length components) 2)
                  (fcd:all-components-touch-common-side-p components minI minJ maxI maxJ))
              "T_CUT"
            )

            ; A single notch from one side creates a П/U-like reserved panel.
            ((and (= (length components) 1)
                  (= (length sideUnion) 1))
              "U_CUT_RESERVED"
            )

            ; Anything with opposite/multiple missing zones is reserved/complex.
            ((or (> (length components) 2)
                 (fcd:opposite-sides-p sideUnion)
                 (> (length sideUnion) 2))
              "U_CUT_RESERVED"
            )

            (T "UNSUPPORTED")
          )
        )
      )
    )
  )
)

(defun fcd:collect-explicit-panel-shapes (ss / shapes i ent layerName pts bbox shapeClass tol)
  (setq shapes nil)
  (setq tol (fcd:get 'tol))
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq layerName (fcd:layer-name-of ent))

    (if
      (and
        (fcd:explicit-shape-layer-p layerName)
        (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
        (fcd:lwpoly-closed-p ent)
      )
      (progn
        (setq pts (fcd:lwpoly->vertices ent))
        (if
          (and
            pts
            (>= (length pts) 4)
            (fcd:polyline-orthogonal-p pts tol)
            (not (fcd:rectangular-poly-p pts tol))
          )
          (progn
            (setq bbox (fcd:bbox-from-pts pts))
            (setq shapeClass (fcd:classify-orthogonal-poly-shape pts))
            (setq shapes
              (cons
                (list
                  (cons 'pts pts)
                  (cons 'bbox bbox)
                  (cons 'class shapeClass)
                  (cons 'layer layerName)
                )
                shapes
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (reverse shapes)
)

(defun fcd:explicit-shape-pts (s)
  (cdr (assoc 'pts s))
)

(defun fcd:explicit-shape-bbox (s)
  (cdr (assoc 'bbox s))
)

(defun fcd:explicit-shape-class (s)
  (cdr (assoc 'class s))
)

(defun fcd:point-in-any-explicit-shape-p (pt shapes tol / hit s)
  (setq hit nil)
  (foreach s shapes
    (if (and (not hit) (fcd:point-in-poly-p pt (fcd:explicit-shape-pts s) tol))
      (setq hit T)
    )
  )
  hit
)

(defun fcd:cell-center-for-exclusion (cell / r)
  (setq r (fcd:cell->panel-rect cell))
  (if r
    (list
      (/ (+ (nth 0 r) (nth 2 r)) 2.0)
      (/ (+ (nth 1 r) (nth 3 r)) 2.0)
    )
    nil
  )
)

(defun fcd:filter-cells-outside-explicit-shapes (cells shapes / out c pt tol)
  (setq out nil)
  (setq tol (fcd:get 'tol))
  (foreach c cells
    (setq pt (fcd:cell-center-for-exclusion c))
    (if
      (or
        (not pt)
        (not (fcd:point-in-any-explicit-shape-p pt shapes tol))
      )
      (setq out (cons c out))
    )
  )
  (reverse out)
)

(defun fcd:preview-explicit-shapes (shapes boundaries openings / s cls pts bbox x1 y1 x2 y2 w h minW minH maxW maxH tol ok warnSmall errBig errBoundary lCut tCut uReserved unsupported)
  (setq ok 0)
  (setq warnSmall 0)
  (setq errBig 0)
  (setq errBoundary 0)
  (setq lCut 0)
  (setq tCut 0)
  (setq uReserved 0)
  (setq unsupported 0)

  (setq minW (fcd:get 'minPanelWidth))
  (setq minH (fcd:get 'minPanelHeight))
  (setq maxW (fcd:get 'maxPanelWidth))
  (setq maxH (fcd:get 'maxPanelHeight))
  (setq tol (fcd:get 'tol))

  (foreach s shapes
    (setq cls (fcd:explicit-shape-class s))
    (setq pts (fcd:explicit-shape-pts s))
    (setq bbox (fcd:explicit-shape-bbox s))
    (setq x1 (nth 0 bbox))
    (setq y1 (nth 1 bbox))
    (setq x2 (nth 2 bbox))
    (setq y2 (nth 3 bbox))
    (setq w (- x2 x1))
    (setq h (- y2 y1))

    (cond
      ((not (fcd:rect-inside-any-boundary-p bbox boundaries tol))
        (fcd:make-poly-preview pts "FCD_PREVIEW_ERR")
        (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
        (fcd:label-in-rect x1 y1 x2 y2 "OUT" "FCD_PREVIEW_ERR")
        (setq errBoundary (1+ errBoundary))
      )

      ((or (< w minW) (< h minH))
        (fcd:make-poly-preview pts "FCD_PREVIEW_WARN")
        (fcd:label-in-rect x1 y1 x2 y2 "SMALL" "FCD_PREVIEW_WARN")
        (setq warnSmall (1+ warnSmall))
      )

      ((or (> w maxW) (> h maxH))
        (fcd:make-poly-preview pts "FCD_PREVIEW_ERR")
        (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
        (fcd:label-in-rect x1 y1 x2 y2 "BIG" "FCD_PREVIEW_ERR")
        (setq errBig (1+ errBig))
      )

      ((= cls "L_CUT")
        (fcd:make-poly-preview pts "FCD_PREVIEW_LCUT")
        (fcd:label-in-rect x1 y1 x2 y2 "L-CUT" "FCD_PREVIEW_LCUT")
        (setq lCut (1+ lCut))
      )

      ((= cls "T_CUT")
        (fcd:make-poly-preview pts "FCD_PREVIEW_TCUT")
        (fcd:label-in-rect x1 y1 x2 y2 "T-CUT" "FCD_PREVIEW_TCUT")
        (setq tCut (1+ tCut))
      )

      ((= cls "U_CUT_RESERVED")
        (fcd:make-poly-preview pts "FCD_PREVIEW_ERR")
        (fcd:label-in-rect x1 y1 x2 y2 "U/P" "FCD_PREVIEW_ERR")
        (setq uReserved (1+ uReserved))
      )

      ((= cls "RECT")
        (fcd:make-poly-preview pts "FCD_PREVIEW_OK")
        (setq ok (1+ ok))
      )

      (T
        (fcd:make-poly-preview pts "FCD_PREVIEW_ERR")
        (fcd:label-in-rect x1 y1 x2 y2 "UNSUP" "FCD_PREVIEW_ERR")
        (setq unsupported (1+ unsupported))
      )
    )
  )

  (list
    (cons 'explicitOk ok)
    (cons 'explicitWarnSmall warnSmall)
    (cons 'explicitErrBig errBig)
    (cons 'explicitErrBoundary errBoundary)
    (cons 'explicitLCut lCut)
    (cons 'explicitTCut tCut)
    (cons 'explicitUReserved uReserved)
    (cons 'explicitUnsupported unsupported)
  )
)

(defun fcd:add-stat (key a b)
  (+
    (cond ((cdr (assoc key a))) (T 0))
    (cond ((cdr (assoc key b))) (T 0))
  )
)

(defun c:FCD_GRID_PREVIEW (/ ss geom boundaries openings grid xs ys vSegs hSegs ignored wrongLayer gridLayer cells filteredCells explicitShapes cellStats shapeStats ok warnSmall errBig errBoundary openingVoid lCut tCut uReserved unsupported invalid eOk eWarn eBig eOut eL eT eU eUns)

  (fcd:init-layers)
  (fcd:clear-preview)

  (setq geom (fcd:scan-geometry))
  (setq boundaries (cdr (assoc 'boundaries geom)))
  (setq openings (cdr (assoc 'openings geom)))

  (fcd:msg
    "Выбери всю геометрию раскладки. Прямоугольные зоны считаются по реальным сторонам, сложные L/T/П — по замкнутым ортогональным LWPOLYLINE. FCD_PREVIEW_* будет пропущен."
  )
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

  (if (not ss)
    (progn
      (fcd:msg "Геометрия не выбрана.")
    )
    (progn
      (setq explicitShapes (fcd:collect-explicit-panel-shapes ss))
      (setq grid (fcd:collect-grid-segments ss))

      (setq xs (cdr (assoc 'xs grid)))
      (setq ys (cdr (assoc 'ys grid)))
      (setq vSegs (cdr (assoc 'vSegs grid)))
      (setq hSegs (cdr (assoc 'hSegs grid)))
      (setq ignored (cdr (assoc 'ignored grid)))
      (setq wrongLayer (cdr (assoc 'wrongLayer grid)))
      (setq gridLayer (cdr (assoc 'gridLayer grid)))

      (setq shapeStats (fcd:preview-explicit-shapes explicitShapes boundaries openings))

      (if (or (< (length xs) 2) (< (length ys) 2))
        (fcd:msg "Недостаточно геометрии для прямоугольной сетки. Сложные замкнутые формы, если были, уже обработаны.")
        (progn
          (setq cells (fcd:build-real-cells xs ys vSegs hSegs))
          (setq filteredCells (fcd:filter-cells-outside-explicit-shapes cells explicitShapes))
          (setq cellStats (fcd:preview-cells filteredCells boundaries openings))

          (setq ok (+ (cdr (assoc 'ok cellStats)) (cdr (assoc 'explicitOk shapeStats))))
          (setq warnSmall (+ (cdr (assoc 'warnSmall cellStats)) (cdr (assoc 'explicitWarnSmall shapeStats))))
          (setq errBig (+ (cdr (assoc 'errBig cellStats)) (cdr (assoc 'explicitErrBig shapeStats))))
          (setq errBoundary (+ (cdr (assoc 'errBoundary cellStats)) (cdr (assoc 'explicitErrBoundary shapeStats))))
          (setq openingVoid (cdr (assoc 'openingVoid cellStats)))
          (setq lCut (+ (cdr (assoc 'lCut cellStats)) (cdr (assoc 'explicitLCut shapeStats))))
          (setq tCut (+ (cdr (assoc 'tCut cellStats)) (cdr (assoc 'explicitTCut shapeStats))))
          (setq uReserved (+ (cdr (assoc 'uReserved cellStats)) (cdr (assoc 'explicitUReserved shapeStats))))
          (setq unsupported (+ (cdr (assoc 'unsupported cellStats)) (cdr (assoc 'explicitUnsupported shapeStats))))
          (setq invalid (cdr (assoc 'invalid cellStats)))

          (fcd:msg
            (strcat
              "Превью создано V10. "
              "источник сетки: " gridLayer
              ", X-координат: " (itoa (length xs))
              ", Y-координат: " (itoa (length ys))
              ", прямоугольных ячеек до фильтра: " (itoa (length cells))
              ", после исключения сложных форм: " (itoa (length filteredCells))
              ", явных сложных форм: " (itoa (length explicitShapes))
              ", OK: " (itoa ok)
              ", маленьких: " (itoa warnSmall)
              ", превышают максимум: " (itoa errBig)
              ", вне границы фасада: " (itoa errBoundary)
              ", L-вырезов/форм: " (itoa lCut)
              ", T-вырезов/форм: " (itoa tCut)
              ", внутри проемов: " (itoa openingVoid)
              ", П/резерв: " (itoa uReserved)
              ", неподдерживаемых: " (itoa unsupported)
              ", невалидных: " (itoa invalid)
              ", проигнорировано сегментов/объектов: " (itoa ignored)
              ", пропущено служебных preview-объектов: " (itoa wrongLayer)
              "."
            )
          )
        )
      )
    )
  )

  (princ)
)

(princ "\nFCD_CORE_V10 overrides loaded: explicit complex L/T/U outlines + stable rectangle grid.")
(princ)


; ============================================================
; V11 OVERRIDES
;
; Decision after tests:
; - Closed explicit polylines are not enough: complex L/T/U panels may be drawn
;   as separate, non-joined LINE/LWPOLYLINE segments.
; - Main panel detection now uses face detection from selected orthogonal linework:
;   atomic grid cells are merged across missing internal edges and split by real
;   drawn edges. This processes unjoined lines as one closed area when the linework
;   is topologically closed.
; - All selected non-preview layers are treated as grid/barrier geometry.
; - FCD_BOUNDARY and FCD_OPENINGS remain constraints and also work as barriers.
; ============================================================

(defun fcd:v11-topology-tol ()
  (max (fcd:get 'tol) 0.1)
)

(defun fcd:v11-shape-point-inside-p (pt shape tol / typ)
  (setq typ (fcd:shape-type shape))
  (cond
    ((= typ "RECT")
      (fcd:point-in-rect-p pt (fcd:shape-rect shape) tol)
    )
    ((= typ "POLY")
      (fcd:point-in-poly-p pt (fcd:shape-pts shape) tol)
    )
    (T nil)
  )
)

(defun fcd:v11-point-in-any-shape-p (pt shapes tol / hit s)
  (setq hit nil)
  (foreach s shapes
    (if (and (not hit) (fcd:v11-shape-point-inside-p pt s tol))
      (setq hit T)
    )
  )
  hit
)

(defun fcd:v11-rect-center (rect)
  (list
    (/ (+ (nth 0 rect) (nth 2 rect)) 2.0)
    (/ (+ (nth 1 rect) (nth 3 rect)) 2.0)
  )
)

(defun fcd:v11-rect-in-opening-p (rect openings tol / pt hit o)
  (setq pt (fcd:v11-rect-center rect))
  (setq hit nil)
  (foreach o openings
    (if
      (and
        (not hit)
        (or
          (fcd:v11-shape-point-inside-p pt o tol)
          (fcd:shape-contains-rect-p o rect tol)
        )
      )
      (setq hit T)
    )
  )
  hit
)

(defun fcd:v11-coords-from-segs (vSegs hSegs / xs ys s tol)
  (setq xs nil)
  (setq ys nil)
  (setq tol (fcd:v11-topology-tol))

  ; vertical segment: (x y1 y2)
  (foreach s vSegs
    (setq xs (fcd:add-unique-number (nth 0 s) xs tol))
    (setq ys (fcd:add-unique-number (nth 1 s) ys tol))
    (setq ys (fcd:add-unique-number (nth 2 s) ys tol))
  )

  ; horizontal segment: (y x1 x2)
  (foreach s hSegs
    (setq ys (fcd:add-unique-number (nth 0 s) ys tol))
    (setq xs (fcd:add-unique-number (nth 1 s) xs tol))
    (setq xs (fcd:add-unique-number (nth 2 s) xs tol))
  )

  (list
    (cons 'xs (fcd:sort-numbers xs))
    (cons 'ys (fcd:sort-numbers ys))
  )
)

(defun fcd:v11-make-cell (i j xs ys)
  (list
    i
    j
    (nth i xs)
    (nth j ys)
    (nth (1+ i) xs)
    (nth (1+ j) ys)
  )
)

(defun fcd:v11-cell-i (c) (nth 0 c))
(defun fcd:v11-cell-j (c) (nth 1 c))
(defun fcd:v11-cell-x1 (c) (nth 2 c))
(defun fcd:v11-cell-y1 (c) (nth 3 c))
(defun fcd:v11-cell-x2 (c) (nth 4 c))
(defun fcd:v11-cell-y2 (c) (nth 5 c))

(defun fcd:v11-cell-rect (c)
  (list
    (fcd:v11-cell-x1 c)
    (fcd:v11-cell-y1 c)
    (fcd:v11-cell-x2 c)
    (fcd:v11-cell-y2 c)
  )
)

(defun fcd:v11-cell-key (c)
  (list (fcd:v11-cell-i c) (fcd:v11-cell-j c))
)

(defun fcd:v11-find-cell (i j cells / hit c)
  (setq hit nil)
  (foreach c cells
    (if (and (not hit) (= i (fcd:v11-cell-i c)) (= j (fcd:v11-cell-j c)))
      (setq hit c)
    )
  )
  hit
)

(defun fcd:v11-build-candidate-cells (xs ys boundaries openings / cells i j c rect tol)
  (setq cells nil)
  (setq tol (fcd:v11-topology-tol))

  (setq i 0)
  (while (< i (1- (length xs)))
    (setq j 0)
    (while (< j (1- (length ys)))
      (setq c (fcd:v11-make-cell i j xs ys))
      (setq rect (fcd:v11-cell-rect c))

      (if
        (and
          (> (- (nth 2 rect) (nth 0 rect)) tol)
          (> (- (nth 3 rect) (nth 1 rect)) tol)
          (fcd:rect-inside-any-boundary-p rect boundaries tol)
          (not (fcd:v11-rect-in-opening-p rect openings tol))
        )
        (setq cells (cons c cells))
      )

      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  (reverse cells)
)

(defun fcd:v11-shared-edge-blocked-p (c1 c2 vSegs hSegs tol / i1 j1 i2 j2 x y x1 x2 y1 y2 left right bottom top)
  (setq i1 (fcd:v11-cell-i c1))
  (setq j1 (fcd:v11-cell-j c1))
  (setq i2 (fcd:v11-cell-i c2))
  (setq j2 (fcd:v11-cell-j c2))

  (cond
    ; horizontal neighbours, shared vertical edge
    ((and (= j1 j2) (= 1 (abs (- i1 i2))))
      (if (< i1 i2)
        (progn (setq left c1) (setq right c2))
        (progn (setq left c2) (setq right c1))
      )
      (setq x (fcd:v11-cell-x2 left))
      (setq y1 (fcd:v11-cell-y1 left))
      (setq y2 (fcd:v11-cell-y2 left))
      (fcd:vertical-covered-p x y1 y2 vSegs tol)
    )

    ; vertical neighbours, shared horizontal edge
    ((and (= i1 i2) (= 1 (abs (- j1 j2))))
      (if (< j1 j2)
        (progn (setq bottom c1) (setq top c2))
        (progn (setq bottom c2) (setq top c1))
      )
      (setq y (fcd:v11-cell-y2 bottom))
      (setq x1 (fcd:v11-cell-x1 bottom))
      (setq x2 (fcd:v11-cell-x2 bottom))
      (fcd:horizontal-covered-p y x1 x2 hSegs tol)
    )

    (T T)
  )
)

(defun fcd:v11-cell-neighbours (cell cells vSegs hSegs tol / i j candidates out n)
  (setq i (fcd:v11-cell-i cell))
  (setq j (fcd:v11-cell-j cell))
  (setq candidates
    (list
      (list (1- i) j)
      (list (1+ i) j)
      (list i (1- j))
      (list i (1+ j))
    )
  )
  (setq out nil)

  (foreach n candidates
    (setq n (fcd:v11-find-cell (car n) (cadr n) cells))
    (if
      (and
        n
        (not (fcd:v11-shared-edge-blocked-p cell n vSegs hSegs tol))
      )
      (setq out (cons n out))
    )
  )

  out
)

(defun fcd:v11-cell-member-p (cell cells / hit c k)
  (setq hit nil)
  (setq k (fcd:v11-cell-key cell))
  (foreach c cells
    (if (and (not hit) (equal k (fcd:v11-cell-key c)))
      (setq hit T)
    )
  )
  hit
)

(defun fcd:v11-remove-cell (cell cells / out c k)
  (setq out nil)
  (setq k (fcd:v11-cell-key cell))
  (foreach c cells
    (if (not (equal k (fcd:v11-cell-key c)))
      (setq out (cons c out))
    )
  )
  (reverse out)
)

(defun fcd:v11-flood-component (start cells vSegs hSegs tol / queue visited c ns n)
  (setq queue (list start))
  (setq visited nil)

  (while queue
    (setq c (car queue))
    (setq queue (cdr queue))

    (if (not (fcd:v11-cell-member-p c visited))
      (progn
        (setq visited (cons c visited))
        (setq ns (fcd:v11-cell-neighbours c cells vSegs hSegs tol))
        (foreach n ns
          (if (not (fcd:v11-cell-member-p n visited))
            (setq queue (cons n queue))
          )
        )
      )
    )
  )

  (reverse visited)
)

(defun fcd:v11-components (cells vSegs hSegs / comps remaining start comp c tol)
  (setq comps nil)
  (setq remaining cells)
  (setq tol (fcd:v11-topology-tol))

  (while remaining
    (setq start (car remaining))
    (setq comp (fcd:v11-flood-component start cells vSegs hSegs tol))
    (setq comps (cons comp comps))
    (foreach c comp
      (setq remaining (fcd:v11-remove-cell c remaining))
    )
  )

  (reverse comps)
)

(defun fcd:v11-pt-less-p (a b)
  (or
    (< (car a) (car b))
    (and (= (car a) (car b)) (< (cadr a) (cadr b)))
  )
)

(defun fcd:v11-normal-edge (a b)
  (if (fcd:v11-pt-less-p a b)
    (list a b)
    (list b a)
  )
)

(defun fcd:v11-toggle-edge (edge edges / out found e)
  (setq out nil)
  (setq found nil)
  (foreach e edges
    (if (equal e edge)
      (setq found T)
      (setq out (cons e out))
    )
  )
  (if found
    (reverse out)
    (cons edge edges)
  )
)

(defun fcd:v11-component-boundary-edges (comp / edges c x1 y1 x2 y2 p1 p2 p3 p4)
  (setq edges nil)
  (foreach c comp
    (setq x1 (fcd:v11-cell-x1 c))
    (setq y1 (fcd:v11-cell-y1 c))
    (setq x2 (fcd:v11-cell-x2 c))
    (setq y2 (fcd:v11-cell-y2 c))
    (setq p1 (list x1 y1))
    (setq p2 (list x2 y1))
    (setq p3 (list x2 y2))
    (setq p4 (list x1 y2))

    (setq edges (fcd:v11-toggle-edge (fcd:v11-normal-edge p1 p2) edges))
    (setq edges (fcd:v11-toggle-edge (fcd:v11-normal-edge p2 p3) edges))
    (setq edges (fcd:v11-toggle-edge (fcd:v11-normal-edge p3 p4) edges))
    (setq edges (fcd:v11-toggle-edge (fcd:v11-normal-edge p4 p1) edges))
  )
  edges
)

(defun fcd:v11-edge-has-point-p (edge pt)
  (or (equal (car edge) pt) (equal (cadr edge) pt))
)

(defun fcd:v11-edge-other-point (edge pt)
  (if (equal (car edge) pt)
    (cadr edge)
    (car edge)
  )
)

(defun fcd:v11-pop-edge-with-point (pt edges / found rest e)
  (setq found nil)
  (setq rest nil)
  (foreach e edges
    (if (and (not found) (fcd:v11-edge-has-point-p e pt))
      (setq found e)
      (setq rest (cons e rest))
    )
  )
  (list found (reverse rest))
)

(defun fcd:v11-trace-one-loop (edges / e rem start cur next result loop guard)
  (setq e (car edges))
  (setq rem (cdr edges))
  (setq start (car e))
  (setq cur (cadr e))
  (setq loop (list start cur))
  (setq guard 0)

  (while (and rem (not (equal cur start)) (< guard 10000))
    (setq result (fcd:v11-pop-edge-with-point cur rem))
    (setq e (car result))
    (setq rem (cadr result))

    (if e
      (progn
        (setq next (fcd:v11-edge-other-point e cur))
        (if (not (equal next start))
          (setq loop (append loop (list next)))
        )
        (setq cur next)
      )
      (setq rem nil)
    )
    (setq guard (1+ guard))
  )

  (list loop rem)
)

(defun fcd:v11-collinear-p (a b c tol)
  (or
    (and (fcd:near= (car a) (car b) tol) (fcd:near= (car b) (car c) tol))
    (and (fcd:near= (cadr a) (cadr b) tol) (fcd:near= (cadr b) (cadr c) tol))
  )
)

(defun fcd:v11-clean-poly-once (pts / out n i prev cur next tol)
  (setq out nil)
  (setq tol (fcd:v11-topology-tol))
  (setq n (length pts))
  (if (< n 3)
    pts
    (progn
      (setq i 0)
      (while (< i n)
        (setq prev (nth (rem (+ i (1- n)) n) pts))
        (setq cur (nth i pts))
        (setq next (nth (rem (1+ i) n) pts))
        (if (not (fcd:v11-collinear-p prev cur next tol))
          (setq out (cons cur out))
        )
        (setq i (1+ i))
      )
      (reverse out)
    )
  )
)

(defun fcd:v11-clean-poly (pts / old new guard)
  (setq old pts)
  (if (and (> (length old) 1) (equal (car old) (last old)))
    (setq old (reverse (cdr (reverse old))))
  )
  (setq guard 0)
  (setq new (fcd:v11-clean-poly-once old))
  (while (and (/= (length new) (length old)) (< guard 20))
    (setq old new)
    (setq new (fcd:v11-clean-poly-once old))
    (setq guard (1+ guard))
  )
  new
)

(defun fcd:v11-trace-loops (edges / loops res loop)
  (setq loops nil)
  (while edges
    (setq res (fcd:v11-trace-one-loop edges))
    (setq loop (fcd:v11-clean-poly (car res)))
    (setq edges (cadr res))
    (if (>= (length loop) 3)
      (setq loops (cons loop loops))
    )
  )
  (reverse loops)
)

(defun fcd:v11-poly-area (pts / sum n i a b)
  (setq sum 0.0)
  (setq n (length pts))
  (if (>= n 3)
    (progn
      (setq i 0)
      (while (< i n)
        (setq a (nth i pts))
        (setq b (nth (rem (1+ i) n) pts))
        (setq sum (+ sum (- (* (car a) (cadr b)) (* (cadr a) (car b)))))
        (setq i (1+ i))
      )
    )
  )
  (/ (abs sum) 2.0)
)


; -----------------------------
; V12 small side detection
; -----------------------------

(defun fcd:v12-max (a b)
  (if (> a b) a b)
)

(defun fcd:v12-min (a b)
  (if (< a b) a b)
)

(defun fcd:v12-dist (a b)
  (sqrt
    (+
      (* (- (car a) (car b)) (- (car a) (car b)))
      (* (- (cadr a) (cadr b)) (- (cadr a) (cadr b)))
    )
  )
)

(defun fcd:v12-small-side-threshold (/ joint minW minH)
  (setq joint (fcd:get 'jointWidth))
  (setq minW (fcd:get 'minPanelWidth))
  (setq minH (fcd:get 'minPanelHeight))
  (fcd:v12-max joint (fcd:v12-min minW minH))
)

(defun fcd:v12-poly-has-small-side-p (pts / threshold tol n i a b len small)
  (setq threshold (fcd:v12-small-side-threshold))
  (setq tol (fcd:get 'tol))
  (setq small nil)
  (setq n (length pts))

  (if (>= n 2)
    (progn
      (setq i 0)
      (while (< i n)
        (setq a (nth i pts))
        (setq b (nth (rem (1+ i) n) pts))
        (setq len (fcd:v12-dist a b))

        ; Any real outer side shorter than max(jointWidth, min panel side)
        ; makes the whole shape non-buildable. This prevents thin joint strips
        ; from being interpreted as valid L/T/U cassettes.
        (if (and (> len tol) (< len (- threshold tol)))
          (setq small T)
        )
        (setq i (1+ i))
      )
    )
  )

  small
)

(defun fcd:v12-shape-small-p (pts bbox / x1 y1 x2 y2 w h minW minH)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq minW (fcd:get 'minPanelWidth))
  (setq minH (fcd:get 'minPanelHeight))

  (or
    (< w minW)
    (< h minH)
    (fcd:v12-poly-has-small-side-p pts)
  )
)

(defun fcd:v12-draw-shape-loops (shape layerName / loops pts l)
  (setq loops (cdr (assoc 'loopPts shape)))
  (if loops
    (foreach l loops
      (fcd:make-poly-preview l layerName)
    )
    (progn
      (setq pts (cdr (assoc 'pts shape)))
      (if pts (fcd:make-poly-preview pts layerName))
    )
  )
)

(defun fcd:v11-largest-loop (loops / best bestA a l)
  (setq best nil)
  (setq bestA -1.0)
  (foreach l loops
    (setq a (fcd:v11-poly-area l))
    (if (> a bestA)
      (progn
        (setq best l)
        (setq bestA a)
      )
    )
  )
  best
)

(defun fcd:v11-component->shape (comp / edges loops main bbox cls)
  (setq edges (fcd:v11-component-boundary-edges comp))
  (setq loops (fcd:v11-trace-loops edges))
  (setq main (fcd:v11-largest-loop loops))
  (if (not main)
    nil
    (progn
      (setq bbox (fcd:bbox-from-pts main))
      (setq cls
        (if (> (length loops) 1)
          "U_CUT_RESERVED"
          (fcd:classify-orthogonal-poly-shape main)
        )
      )
      (list
        (cons 'pts main)
        (cons 'bbox bbox)
        (cons 'class cls)
        (cons 'loops (length loops))
        (cons 'loopPts loops)
        (cons 'cells (length comp))
      )
    )
  )
)

(defun fcd:v11-preview-shapes (shapes / s cls pts bbox x1 y1 x2 y2 w h minW minH maxW maxH ok warnSmall errBig lCut tCut uReserved unsupported invalid)
  (setq ok 0)
  (setq warnSmall 0)
  (setq errBig 0)
  (setq lCut 0)
  (setq tCut 0)
  (setq uReserved 0)
  (setq unsupported 0)
  (setq invalid 0)

  (setq minW (fcd:get 'minPanelWidth))
  (setq minH (fcd:get 'minPanelHeight))
  (setq maxW (fcd:get 'maxPanelWidth))
  (setq maxH (fcd:get 'maxPanelHeight))

  (foreach s shapes
    (if (not s)
      (setq invalid (1+ invalid))
      (progn
        (setq cls (cdr (assoc 'class s)))
        (setq pts (cdr (assoc 'pts s)))
        (setq bbox (cdr (assoc 'bbox s)))
        (setq x1 (nth 0 bbox))
        (setq y1 (nth 1 bbox))
        (setq x2 (nth 2 bbox))
        (setq y2 (nth 3 bbox))
        (setq w (- x2 x1))
        (setq h (- y2 y1))

        (cond
          ((fcd:v12-shape-small-p pts bbox)
            (fcd:v12-draw-shape-loops s "FCD_PREVIEW_WARN")
            (fcd:label-in-rect x1 y1 x2 y2 "SMALL" "FCD_PREVIEW_WARN")
            (setq warnSmall (1+ warnSmall))
          )

          ((or (> w maxW) (> h maxH))
            (fcd:make-poly-preview pts "FCD_PREVIEW_ERR")
            (fcd:make-cross-in-rect x1 y1 x2 y2 "FCD_PREVIEW_ERR")
            (fcd:label-in-rect x1 y1 x2 y2 "BIG" "FCD_PREVIEW_ERR")
            (setq errBig (1+ errBig))
          )

          ((= cls "RECT")
            (fcd:make-poly-preview pts "FCD_PREVIEW_OK")
            (setq ok (1+ ok))
          )

          ((= cls "L_CUT")
            (fcd:make-poly-preview pts "FCD_PREVIEW_LCUT")
            (fcd:label-in-rect x1 y1 x2 y2 "L-CUT" "FCD_PREVIEW_LCUT")
            (setq lCut (1+ lCut))
          )

          ((= cls "T_CUT")
            (fcd:make-poly-preview pts "FCD_PREVIEW_TCUT")
            (fcd:label-in-rect x1 y1 x2 y2 "T-CUT" "FCD_PREVIEW_TCUT")
            (setq tCut (1+ tCut))
          )

          ((= cls "U_CUT_RESERVED")
            (fcd:v12-draw-shape-loops s "FCD_PREVIEW_UCUT")
            (fcd:label-in-rect x1 y1 x2 y2 "U/P" "FCD_PREVIEW_UCUT")
            (setq uReserved (1+ uReserved))
          )

          (T
            (fcd:make-poly-preview pts "FCD_PREVIEW_ERR")
            (fcd:label-in-rect x1 y1 x2 y2 "UNSUP" "FCD_PREVIEW_ERR")
            (setq unsupported (1+ unsupported))
          )
        )
      )
    )
  )

  (list
    (cons 'ok ok)
    (cons 'warnSmall warnSmall)
    (cons 'errBig errBig)
    (cons 'lCut lCut)
    (cons 'tCut tCut)
    (cons 'uReserved uReserved)
    (cons 'unsupported unsupported)
    (cons 'invalid invalid)
  )
)

(defun fcd:v11-opening-void-preview (openings / count o typ rect pts bbox)
  (setq count 0)
  (foreach o openings
    (setq typ (fcd:shape-type o))
    (cond
      ((= typ "RECT")
        (setq rect (fcd:shape-rect o))
        (fcd:make-rect-polyline (nth 0 rect) (nth 1 rect) (nth 2 rect) (nth 3 rect) "FCD_PREVIEW_VOID")
        (fcd:make-cross-in-rect (nth 0 rect) (nth 1 rect) (nth 2 rect) (nth 3 rect) "FCD_PREVIEW_VOID")
        (fcd:label-in-rect (nth 0 rect) (nth 1 rect) (nth 2 rect) (nth 3 rect) "VOID" "FCD_PREVIEW_VOID")
        (setq count (1+ count))
      )
      ((= typ "POLY")
        (setq pts (fcd:shape-pts o))
        (setq bbox (fcd:shape-bbox o))
        (fcd:make-poly-preview pts "FCD_PREVIEW_VOID")
        (fcd:label-in-rect (nth 0 bbox) (nth 1 bbox) (nth 2 bbox) (nth 3 bbox) "VOID" "FCD_PREVIEW_VOID")
        (setq count (1+ count))
      )
    )
  )
  count
)

(defun c:FCD_GRID_PREVIEW (/ ss geom boundaries openings grid vSegs hSegs ignored wrongLayer coordData xs ys cells comps shapes cStats voidCount stats ok warnSmall errBig lCut tCut uReserved unsupported invalid)
  (fcd:init-layers)
  (fcd:clear-preview)

  (setq geom (fcd:scan-geometry))
  (setq boundaries (cdr (assoc 'boundaries geom)))
  (setq openings (cdr (assoc 'openings geom)))

  (fcd:msg
    "Выбери всю геометрию раскладки. V12: все выбранные LINE/LWPOLYLINE считаются барьерами сетки; маленькие стороны L/T/U-форм помечаются как SMALL."
  )
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

  (if (not ss)
    (fcd:msg "Геометрия не выбрана.")
    (progn
      (setq grid (fcd:collect-grid-segments ss))
      (setq vSegs (cdr (assoc 'vSegs grid)))
      (setq hSegs (cdr (assoc 'hSegs grid)))
      (setq ignored (cdr (assoc 'ignored grid)))
      (setq wrongLayer (cdr (assoc 'wrongLayer grid)))

      (setq coordData (fcd:v11-coords-from-segs vSegs hSegs))
      (setq xs (cdr (assoc 'xs coordData)))
      (setq ys (cdr (assoc 'ys coordData)))

      (if (or (< (length xs) 2) (< (length ys) 2))
        (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
        (progn
          (setq cells (fcd:v11-build-candidate-cells xs ys boundaries openings))
          (setq comps (fcd:v11-components cells vSegs hSegs))
          (setq shapes nil)
          (foreach c comps
            (setq shapes (cons (fcd:v11-component->shape c) shapes))
          )
          (setq shapes (reverse shapes))

          (setq voidCount (fcd:v11-opening-void-preview openings))
          (setq stats (fcd:v11-preview-shapes shapes))

          (setq ok (cdr (assoc 'ok stats)))
          (setq warnSmall (cdr (assoc 'warnSmall stats)))
          (setq errBig (cdr (assoc 'errBig stats)))
          (setq lCut (cdr (assoc 'lCut stats)))
          (setq tCut (cdr (assoc 'tCut stats)))
          (setq uReserved (cdr (assoc 'uReserved stats)))
          (setq unsupported (cdr (assoc 'unsupported stats)))
          (setq invalid (cdr (assoc 'invalid stats)))

          (fcd:msg
            (strcat
              "Превью создано V12. "
              "X-координат: " (itoa (length xs))
              ", Y-координат: " (itoa (length ys))
              ", атомарных зон внутри фасада: " (itoa (length cells))
              ", найдено областей/панелей: " (itoa (length comps))
              ", OK: " (itoa ok)
              ", маленьких: " (itoa warnSmall)
              ", превышают максимум: " (itoa errBig)
              ", L-форм: " (itoa lCut)
              ", T-форм: " (itoa tCut)
              ", VOID-проемов: " (itoa voidCount)
              ", П/резерв: " (itoa uReserved)
              ", неподдерживаемых: " (itoa unsupported)
              ", невалидных: " (itoa invalid)
              ", проигнорировано объектов/сегментов: " (itoa ignored)
              ", пропущено preview/служебных: " (itoa wrongLayer)
              "."
            )
          )
        )
      )
    )
  )

  (princ)
)

(princ "\nFCD_CORE_V12 loaded: improved small-side detection and reserved U/P preview.")
(princ)

; ============================================================
; V14 PANEL BUILD RECT
;
; Decision:
; - Fixed block name only: FCD_Panel.
; - No command for renaming / reassigning the panel block.
; - Build stage inserts only valid RECT areas.
; - L/T/U/SMALL/BIG areas are skipped for now.
; - Block is measured once, then inserted into every suitable rectangle
;   with non-uniform X/Y scale and bbox alignment.
;
; Commands:
; - FCD_CLEAR_PANELS
; - FCD_PANEL_BUILD_RECT
; ============================================================

(defun fcd:v14-panel-block-name ()
  "FCD_Panel"
)

(defun fcd:v14-active-space (/ acad doc)
  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad))
  (if (= (getvar "CVPORT") 1)
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)
  )
)

(defun fcd:v14-block-exists-p (/ blockName)
  (setq blockName (fcd:v14-panel-block-name))
  (if (tblsearch "BLOCK" blockName) T nil)
)

(defun fcd:v14-vpt (pt)
  (vlax-3d-point (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0)))
)

(defun fcd:v14-bbox (obj / res minpt maxpt pmin pmax)
  (setq res (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt)))
  (if (vl-catch-all-error-p res)
    nil
    (progn
      (setq pmin (fcd:variant-point->list minpt))
      (setq pmax (fcd:variant-point->list maxpt))
      (list pmin pmax)
    )
  )
)

(defun fcd:v14-bbox-min-x (bbox) (car (car bbox)))
(defun fcd:v14-bbox-min-y (bbox) (cadr (car bbox)))
(defun fcd:v14-bbox-max-x (bbox) (car (cadr bbox)))
(defun fcd:v14-bbox-max-y (bbox) (cadr (cadr bbox)))

(defun fcd:v14-bbox-width (bbox)
  (- (fcd:v14-bbox-max-x bbox) (fcd:v14-bbox-min-x bbox))
)

(defun fcd:v14-bbox-height (bbox)
  (- (fcd:v14-bbox-max-y bbox) (fcd:v14-bbox-min-y bbox))
)

(defun fcd:v14-delete-object (obj)
  (if obj
    (vl-catch-all-apply 'vla-Delete (list obj))
  )
)

(defun fcd:v14-insert-raw (blockName insPt sx sy / space obj)
  (setq space (fcd:v14-active-space))
  (setq obj
    (vl-catch-all-apply
      'vla-InsertBlock
      (list space (fcd:v14-vpt insPt) blockName sx sy 1.0 0.0)
    )
  )
  (if (vl-catch-all-error-p obj)
    nil
    obj
  )
)

(defun fcd:v14-measure-block (/ blockName tmp bbox bw bh result)
  (setq blockName (fcd:v14-panel-block-name))
  (setq result nil)

  (setq tmp (fcd:v14-insert-raw blockName (list 0.0 0.0 0.0) 1.0 1.0))

  (if tmp
    (progn
      (vl-catch-all-apply 'vla-put-Layer (list tmp "FCD_PANELS"))
      (vl-catch-all-apply 'vla-Update (list tmp))
      (setq bbox (fcd:v14-bbox tmp))

      (if bbox
        (progn
          (setq bw (fcd:v14-bbox-width bbox))
          (setq bh (fcd:v14-bbox-height bbox))
          (if (and (> bw (fcd:get 'tol)) (> bh (fcd:get 'tol)))
            (setq result
              (list
                (cons 'minX (fcd:v14-bbox-min-x bbox))
                (cons 'minY (fcd:v14-bbox-min-y bbox))
                (cons 'maxX (fcd:v14-bbox-max-x bbox))
                (cons 'maxY (fcd:v14-bbox-max-y bbox))
                (cons 'width bw)
                (cons 'height bh)
              )
            )
          )
        )
      )

      (fcd:v14-delete-object tmp)
    )
  )

  result
)

(defun fcd:v14-move-object (obj dx dy)
  (vl-catch-all-apply
    'vla-Move
    (list obj (fcd:v14-vpt (list 0.0 0.0 0.0)) (fcd:v14-vpt (list dx dy 0.0)))
  )
)

(defun fcd:v14-align-bbox-min-to-rect (obj x1 y1 / bbox dx dy)
  (setq bbox (fcd:v14-bbox obj))
  (if bbox
    (progn
      (setq dx (- x1 (fcd:v14-bbox-min-x bbox)))
      (setq dy (- y1 (fcd:v14-bbox-min-y bbox)))
      (fcd:v14-move-object obj dx dy)
      (vl-catch-all-apply 'vla-Update (list obj))
      T
    )
    nil
  )
)

(defun fcd:v14-refit-block-to-rect (obj x1 y1 x2 y2 / bbox cw ch tw th xs ys oldXs oldYs tol)
  (setq tol (fcd:get 'tol))
  (setq tw (- x2 x1))
  (setq th (- y2 y1))
  (setq bbox (fcd:v14-bbox obj))

  (if (and bbox (> tw tol) (> th tol))
    (progn
      (setq cw (fcd:v14-bbox-width bbox))
      (setq ch (fcd:v14-bbox-height bbox))

      (if (and (> cw tol) (> ch tol))
        (progn
          (setq oldXs (vla-get-XScaleFactor obj))
          (setq oldYs (vla-get-YScaleFactor obj))
          (setq xs (* oldXs (/ tw cw)))
          (setq ys (* oldYs (/ th ch)))

          (vl-catch-all-apply 'vla-put-XScaleFactor (list obj xs))
          (vl-catch-all-apply 'vla-put-YScaleFactor (list obj ys))
          (vl-catch-all-apply 'vla-put-ZScaleFactor (list obj 1.0))
          (vl-catch-all-apply 'vla-Update (list obj))
          (fcd:v14-align-bbox-min-to-rect obj x1 y1)
          T
        )
        nil
      )
    )
    nil
  )
)

(defun fcd:v14-fit-block-to-rect (blockInfo x1 y1 x2 y2 / blockName bw bh minX minY tw th sx sy insX insY obj ok)
  (setq blockName (fcd:v14-panel-block-name))
  (setq bw (cdr (assoc 'width blockInfo)))
  (setq bh (cdr (assoc 'height blockInfo)))
  (setq minX (cdr (assoc 'minX blockInfo)))
  (setq minY (cdr (assoc 'minY blockInfo)))
  (setq tw (- x2 x1))
  (setq th (- y2 y1))

  (setq obj nil)

  (if (and (> bw (fcd:get 'tol)) (> bh (fcd:get 'tol)) (> tw (fcd:get 'tol)) (> th (fcd:get 'tol)))
    (progn
      (setq sx (/ tw bw))
      (setq sy (/ th bh))

      ; Insertion point is compensated by original block bbox offset.
      ; This prevents random-looking placement when block base point is not its lower-left corner.
      (setq insX (- x1 (* minX sx)))
      (setq insY (- y1 (* minY sy)))

      (setq obj (fcd:v14-insert-raw blockName (list insX insY 0.0) sx sy))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (vl-catch-all-apply 'vla-Update (list obj))

          ; Two-pass bbox correction. This makes insertion robust even if
          ; attributes / dynamic geometry affect bbox after first scaling.
          (setq ok (fcd:v14-align-bbox-min-to-rect obj x1 y1))
          (if ok (setq ok (fcd:v14-refit-block-to-rect obj x1 y1 x2 y2)))
          (if ok (setq ok (fcd:v14-refit-block-to-rect obj x1 y1 x2 y2)))

          (if ok
            obj
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v14-shapes-from-selection (ss / geom boundaries openings grid vSegs hSegs coordData xs ys cells comps shapes c ignored wrongLayer)
  (setq geom (fcd:scan-geometry))
  (setq boundaries (cdr (assoc 'boundaries geom)))
  (setq openings (cdr (assoc 'openings geom)))

  (setq grid (fcd:collect-grid-segments ss))
  (setq vSegs (cdr (assoc 'vSegs grid)))
  (setq hSegs (cdr (assoc 'hSegs grid)))
  (setq ignored (cdr (assoc 'ignored grid)))
  (setq wrongLayer (cdr (assoc 'wrongLayer grid)))

  (setq coordData (fcd:v11-coords-from-segs vSegs hSegs))
  (setq xs (cdr (assoc 'xs coordData)))
  (setq ys (cdr (assoc 'ys coordData)))

  (if (or (< (length xs) 2) (< (length ys) 2))
    (list
      (cons 'ok nil)
      (cons 'reason "NOT_ENOUGH_GEOMETRY")
      (cons 'shapes nil)
      (cons 'ignored ignored)
      (cons 'wrongLayer wrongLayer)
      (cons 'xs xs)
      (cons 'ys ys)
    )
    (progn
      (setq cells (fcd:v11-build-candidate-cells xs ys boundaries openings))
      (setq comps (fcd:v11-components cells vSegs hSegs))
      (setq shapes nil)
      (foreach c comps
        (setq shapes (cons (fcd:v11-component->shape c) shapes))
      )
      (setq shapes (reverse shapes))

      (list
        (cons 'ok T)
        (cons 'reason nil)
        (cons 'shapes shapes)
        (cons 'cells cells)
        (cons 'comps comps)
        (cons 'ignored ignored)
        (cons 'wrongLayer wrongLayer)
        (cons 'xs xs)
        (cons 'ys ys)
      )
    )
  )
)

(defun fcd:v14-suitable-rect-p (shape / cls pts bbox x1 y1 x2 y2 w h maxW maxH)
  (if (not shape)
    nil
    (progn
      (setq cls (cdr (assoc 'class shape)))
      (setq pts (cdr (assoc 'pts shape)))
      (setq bbox (cdr (assoc 'bbox shape)))
      (if (not bbox)
        nil
        (progn
          (setq x1 (nth 0 bbox))
          (setq y1 (nth 1 bbox))
          (setq x2 (nth 2 bbox))
          (setq y2 (nth 3 bbox))
          (setq w (- x2 x1))
          (setq h (- y2 y1))
          (setq maxW (fcd:get 'maxPanelWidth))
          (setq maxH (fcd:get 'maxPanelHeight))
          (and
            (= cls "RECT")
            (not (fcd:v12-shape-small-p pts bbox))
            (<= w maxW)
            (<= h maxH)
          )
        )
      )
    )
  )
)

(defun fcd:v14-build-rect-blocks (shapes blockInfo / inserted skippedNonRect skippedSmall skippedBig failed invalid shape cls pts bbox x1 y1 x2 y2 w h maxW maxH obj)
  (setq inserted 0)
  (setq skippedNonRect 0)
  (setq skippedSmall 0)
  (setq skippedBig 0)
  (setq failed 0)
  (setq invalid 0)
  (setq maxW (fcd:get 'maxPanelWidth))
  (setq maxH (fcd:get 'maxPanelHeight))

  (foreach shape shapes
    (if (not shape)
      (setq invalid (1+ invalid))
      (progn
        (setq cls (cdr (assoc 'class shape)))
        (setq pts (cdr (assoc 'pts shape)))
        (setq bbox (cdr (assoc 'bbox shape)))

        (if (not bbox)
          (setq invalid (1+ invalid))
          (progn
            (setq x1 (nth 0 bbox))
            (setq y1 (nth 1 bbox))
            (setq x2 (nth 2 bbox))
            (setq y2 (nth 3 bbox))
            (setq w (- x2 x1))
            (setq h (- y2 y1))

            (cond
              ((not (= cls "RECT"))
                (setq skippedNonRect (1+ skippedNonRect))
              )
              ((fcd:v12-shape-small-p pts bbox)
                (setq skippedSmall (1+ skippedSmall))
              )
              ((or (> w maxW) (> h maxH))
                (setq skippedBig (1+ skippedBig))
              )
              (T
                (setq obj (fcd:v14-fit-block-to-rect blockInfo x1 y1 x2 y2))
                (if obj
                  (setq inserted (1+ inserted))
                  (setq failed (1+ failed))
                )
              )
            )
          )
        )
      )
    )
  )

  (list
    (cons 'inserted inserted)
    (cons 'skippedNonRect skippedNonRect)
    (cons 'skippedSmall skippedSmall)
    (cons 'skippedBig skippedBig)
    (cons 'failed failed)
    (cons 'invalid invalid)
  )
)

(defun c:FCD_CLEAR_PANELS ()
  (fcd:delete-layer-objects "FCD_PANELS")
  (fcd:msg "Блоки/объекты на слое FCD_PANELS удалены.")
  (princ)
)

(defun c:FCD_PANEL_BUILD_RECT (/ ss data shapes blockInfo stats inserted skippedNonRect skippedSmall skippedBig failed invalid blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай образец блока с точным именем FCD_Panel.")
    (progn
      (setq blockInfo (fcd:v14-measure-block))

      (if (not blockInfo)
        (fcd:msg "Не удалось измерить FCD_Panel. Проверь, что блок содержит видимую геометрию с ненулевой шириной и высотой.")
        (progn
          (fcd:msg
            (strcat
              "Выбери всю геометрию раскладки. Будут вставлены только подходящие RECT-области блоком "
              blockName
              "."
            )
          )
          (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

          (if (not ss)
            (fcd:msg "Геометрия не выбрана.")
            (progn
              (setq data (fcd:v14-shapes-from-selection ss))

              (if (not (cdr (assoc 'ok data)))
                (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
                (progn
                  (setq shapes (cdr (assoc 'shapes data)))
                  (setq stats (fcd:v14-build-rect-blocks shapes blockInfo))

                  (setq inserted (cdr (assoc 'inserted stats)))
                  (setq skippedNonRect (cdr (assoc 'skippedNonRect stats)))
                  (setq skippedSmall (cdr (assoc 'skippedSmall stats)))
                  (setq skippedBig (cdr (assoc 'skippedBig stats)))
                  (setq failed (cdr (assoc 'failed stats)))
                  (setq invalid (cdr (assoc 'invalid stats)))

                  (fcd:msg
                    (strcat
                      "Вставка RECT завершена. "
                      "Вставлено: " (itoa inserted)
                      ", пропущено L/T/U/прочих: " (itoa skippedNonRect)
                      ", SMALL: " (itoa skippedSmall)
                      ", BIG: " (itoa skippedBig)
                      ", ошибок вставки: " (itoa failed)
                      ", невалидных: " (itoa invalid)
                      ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                      ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                      "."
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(princ "\nFCD_CORE_V14 loaded: fixed FCD_Panel block insertion into all suitable RECT areas.")
(princ)

; ============================================================
; V15 OVERRIDES
;
; Fixes:
; - AutoCAD sometimes returns GetBoundingBox points as SAFEARRAY, not VARIANT.
;   V14 assumed VARIANT and failed with: "bad argument type: variantp #<safearray...>".
; - Temporary measuring insert is now deleted even when bbox reading fails.
; - FCD_PANEL_BUILD_RECT should no longer leave one random temporary block.
; ============================================================

(defun fcd:v15-point-data->list (p / tv)
  (setq tv (type p))
  (cond
    ((= tv 'VARIANT)
      (vlax-safearray->list (vlax-variant-value p))
    )
    ((= tv 'SAFEARRAY)
      (vlax-safearray->list p)
    )
    ((listp p)
      p
    )
    (T nil)
  )
)

(defun fcd:v14-bbox (obj / res minpt maxpt pmin pmax)
  (setq res (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt)))
  (if (vl-catch-all-error-p res)
    nil
    (progn
      (setq pmin (fcd:v15-point-data->list minpt))
      (setq pmax (fcd:v15-point-data->list maxpt))
      (if (and pmin pmax)
        (list pmin pmax)
        nil
      )
    )
  )
)

(defun fcd:v14-measure-block (/ blockName tmp bbox bw bh result)
  (setq blockName (fcd:v14-panel-block-name))
  (setq result nil)
  (setq tmp nil)

  (setq tmp (fcd:v14-insert-raw blockName (list 0.0 0.0 0.0) 1.0 1.0))

  (if tmp
    (progn
      (vl-catch-all-apply 'vla-put-Layer (list tmp "FCD_PANELS"))
      (vl-catch-all-apply 'vla-Update (list tmp))
      (setq bbox (fcd:v14-bbox tmp))

      (if bbox
        (progn
          (setq bw (fcd:v14-bbox-width bbox))
          (setq bh (fcd:v14-bbox-height bbox))
          (if (and (> bw (fcd:get 'tol)) (> bh (fcd:get 'tol)))
            (setq result
              (list
                (cons 'minX (fcd:v14-bbox-min-x bbox))
                (cons 'minY (fcd:v14-bbox-min-y bbox))
                (cons 'maxX (fcd:v14-bbox-max-x bbox))
                (cons 'maxY (fcd:v14-bbox-max-y bbox))
                (cons 'width bw)
                (cons 'height bh)
              )
            )
          )
        )
      )

      ; Always delete the temporary measuring insert.
      (fcd:v14-delete-object tmp)
    )
  )

  result
)

(defun c:FCD_PANEL_BUILD_RECT (/ ss data shapes blockInfo stats inserted skippedNonRect skippedSmall skippedBig failed invalid blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай образец блока с точным именем FCD_Panel.")
    (progn
      (setq blockInfo (fcd:v14-measure-block))

      (if (not blockInfo)
        (fcd:msg "Не удалось измерить FCD_Panel. Проверь, что блок содержит видимую геометрию с ненулевой шириной и высотой.")
        (progn
          (fcd:msg
            (strcat
              "Выбери всю геометрию раскладки. Будут вставлены только подходящие RECT-области блоком "
              blockName
              "."
            )
          )
          (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

          (if (not ss)
            (fcd:msg "Геометрия не выбрана.")
            (progn
              (setq data (fcd:v14-shapes-from-selection ss))

              (if (not (cdr (assoc 'ok data)))
                (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
                (progn
                  (setq shapes (cdr (assoc 'shapes data)))
                  (setq stats (fcd:v14-build-rect-blocks shapes blockInfo))

                  (setq inserted (cdr (assoc 'inserted stats)))
                  (setq skippedNonRect (cdr (assoc 'skippedNonRect stats)))
                  (setq skippedSmall (cdr (assoc 'skippedSmall stats)))
                  (setq skippedBig (cdr (assoc 'skippedBig stats)))
                  (setq failed (cdr (assoc 'failed stats)))
                  (setq invalid (cdr (assoc 'invalid stats)))

                  (fcd:msg
                    (strcat
                      "Вставка RECT завершена V15. "
                      "Вставлено: " (itoa inserted)
                      ", пропущено L/T/U/прочих: " (itoa skippedNonRect)
                      ", SMALL: " (itoa skippedSmall)
                      ", BIG: " (itoa skippedBig)
                      ", ошибок вставки: " (itoa failed)
                      ", невалидных: " (itoa invalid)
                      ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                      ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                      "."
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(princ "\nFCD_CORE_V15 loaded: fixed SAFEARRAY bbox error and RECT block insertion measurement.")
(princ)

; ============================================================
; V16 DYNAMIC PANEL INSERTION OVERRIDES
;
; Changes:
; - FCD_PANEL_BUILD_RECT no longer uses X/Y scale to fit FCD_Panel.
; - Every block is inserted with scale 1.0 / 1.0 / 1.0.
; - Rectangle size is written into dynamic block properties:
;     L = panel width
;     H = panel height
; - This preserves dynamic block behaviour, attributes, constraints,
;   formulas and nested geometry much better than non-uniform scaling.
;
; Required block:
; - FCD_Panel
;
; Required dynamic properties in block:
; - L
; - H
; ============================================================

(defun fcd:v16-str= (a b)
  (= (strcase a) (strcase b))
)

(defun fcd:v16-prop-name-in-list-p (name names / ok n)
  (setq ok nil)
  (foreach n names
    (if (fcd:v16-str= name n)
      (setq ok T)
    )
  )
  ok
)

(defun fcd:v16-get-dynamic-properties (obj / res)
  (setq res (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p res)
    nil
    res
  )
)

(defun fcd:v16-dynprop-readonly-p (prop / res)
  (setq res (vl-catch-all-apply 'vlax-get-property (list prop 'ReadOnly)))
  (if (vl-catch-all-error-p res)
    nil
    (if (= res :vlax-true) T nil)
  )
)

(defun fcd:v16-dynprop-name (prop / res)
  (setq res (vl-catch-all-apply 'vlax-get-property (list prop 'PropertyName)))
  (if (vl-catch-all-error-p res)
    nil
    res
  )
)

(defun fcd:v16-set-dynprop-by-aliases (obj aliases value / props prop pname found setOk res)
  (setq found nil)
  (setq setOk nil)
  (setq props (fcd:v16-get-dynamic-properties obj))

  (if props
    (foreach prop props
      (if (not setOk)
        (progn
          (setq pname (fcd:v16-dynprop-name prop))
          (if
            (and
              pname
              (fcd:v16-prop-name-in-list-p pname aliases)
              (not (fcd:v16-dynprop-readonly-p prop))
            )
            (progn
              (setq found T)
              (setq res (vl-catch-all-apply 'vlax-put-property (list prop 'Value value)))
              (if (not (vl-catch-all-error-p res))
                (setq setOk T)
              )
            )
          )
        )
      )
    )
  )

  (list
    (cons 'found found)
    (cons 'set setOk)
  )
)

(defun fcd:v16-set-panel-dynamic-size (obj width height / lres hres lset hset)
  ; Primary required property names are exactly L and H.
  ; Extra aliases are accepted only to make the tool more tolerant if
  ; the sample block later gets renamed parameters.
  (setq lres
    (fcd:v16-set-dynprop-by-aliases
      obj
      (list "L" "PANEL_L" "PANEL_WIDTH" "WIDTH" "ДЛИНА" "ШИРИНА")
      width
    )
  )

  (setq hres
    (fcd:v16-set-dynprop-by-aliases
      obj
      (list "H" "PANEL_H" "PANEL_HEIGHT" "HEIGHT" "ВЫСОТА")
      height
    )
  )

  (setq lset (cdr (assoc 'set lres)))
  (setq hset (cdr (assoc 'set hres)))

  (list
    (cons 'lFound (cdr (assoc 'found lres)))
    (cons 'hFound (cdr (assoc 'found hres)))
    (cons 'lSet lset)
    (cons 'hSet hset)
    (cons 'ok (and lset hset))
  )
)

(defun fcd:v16-reset-block-scale (obj)
  (vl-catch-all-apply 'vla-put-XScaleFactor (list obj 1.0))
  (vl-catch-all-apply 'vla-put-YScaleFactor (list obj 1.0))
  (vl-catch-all-apply 'vla-put-ZScaleFactor (list obj 1.0))
  T
)

(defun fcd:v16-insert-dynamic-panel-in-rect (x1 y1 x2 y2 / blockName tw th obj sizeResult ok)
  (setq blockName (fcd:v14-panel-block-name))
  (setq tw (- x2 x1))
  (setq th (- y2 y1))
  (setq obj nil)
  (setq ok nil)

  (if (and (> tw (fcd:get 'tol)) (> th (fcd:get 'tol)))
    (progn
      ; Insert at lower-left first. After dynamic properties are set,
      ; bbox is aligned to the rectangle without changing scale.
      (setq obj (fcd:v14-insert-raw blockName (list x1 y1 0.0) 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)

          (setq sizeResult (fcd:v16-set-panel-dynamic-size obj tw th))

          (if (cdr (assoc 'ok sizeResult))
            (progn
              (vl-catch-all-apply 'vla-Update (list obj))
              ; Move only. Do not scale. This keeps dynamic constraints and
              ; attribute behaviour intact.
              (setq ok (fcd:v14-align-bbox-min-to-rect obj x1 y1))
              (vl-catch-all-apply 'vla-Update (list obj))
            )
          )

          (if ok
            obj
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v16-build-rect-blocks (shapes / inserted skippedNonRect skippedSmall skippedBig failed invalid shape cls pts bbox x1 y1 x2 y2 w h maxW maxH obj)
  (setq inserted 0)
  (setq skippedNonRect 0)
  (setq skippedSmall 0)
  (setq skippedBig 0)
  (setq failed 0)
  (setq invalid 0)
  (setq maxW (fcd:get 'maxPanelWidth))
  (setq maxH (fcd:get 'maxPanelHeight))

  (foreach shape shapes
    (if (not shape)
      (setq invalid (1+ invalid))
      (progn
        (setq cls (cdr (assoc 'class shape)))
        (setq pts (cdr (assoc 'pts shape)))
        (setq bbox (cdr (assoc 'bbox shape)))

        (if (not bbox)
          (setq invalid (1+ invalid))
          (progn
            (setq x1 (nth 0 bbox))
            (setq y1 (nth 1 bbox))
            (setq x2 (nth 2 bbox))
            (setq y2 (nth 3 bbox))
            (setq w (- x2 x1))
            (setq h (- y2 y1))

            (cond
              ((not (= cls "RECT"))
                (setq skippedNonRect (1+ skippedNonRect))
              )
              ((fcd:v12-shape-small-p pts bbox)
                (setq skippedSmall (1+ skippedSmall))
              )
              ((or (> w maxW) (> h maxH))
                (setq skippedBig (1+ skippedBig))
              )
              (T
                (setq obj (fcd:v16-insert-dynamic-panel-in-rect x1 y1 x2 y2))
                (if obj
                  (setq inserted (1+ inserted))
                  (setq failed (1+ failed))
                )
              )
            )
          )
        )
      )
    )
  )

  (list
    (cons 'inserted inserted)
    (cons 'skippedNonRect skippedNonRect)
    (cons 'skippedSmall skippedSmall)
    (cons 'skippedBig skippedBig)
    (cons 'failed failed)
    (cons 'invalid invalid)
  )
)

(defun c:FCD_PANEL_BUILD_RECT (/ ss data shapes stats inserted skippedNonRect skippedSmall skippedBig failed invalid blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай образец блока с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. Будут вставлены только подходящие RECT-области блоком "
          blockName
          " через динамические свойства L/H, без масштабирования."
        )
      )
      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v16-build-rect-blocks shapes))

              (setq inserted (cdr (assoc 'inserted stats)))
              (setq skippedNonRect (cdr (assoc 'skippedNonRect stats)))
              (setq skippedSmall (cdr (assoc 'skippedSmall stats)))
              (setq skippedBig (cdr (assoc 'skippedBig stats)))
              (setq failed (cdr (assoc 'failed stats)))
              (setq invalid (cdr (assoc 'invalid stats)))

              (fcd:msg
                (strcat
                  "Вставка RECT завершена V16. "
                  "Вставлено: " (itoa inserted)
                  ", пропущено L/T/U/прочих: " (itoa skippedNonRect)
                  ", SMALL: " (itoa skippedSmall)
                  ", BIG: " (itoa skippedBig)
                  ", ошибок вставки: " (itoa failed)
                  ", невалидных: " (itoa invalid)
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (if (> failed 0)
                (fcd:msg "Если ошибок вставки больше 0, проверь, что динамический блок FCD_Panel имеет редактируемые свойства L и H.")
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(princ "\nFCD_CORE_V16 loaded: RECT insertion uses dynamic properties L/H, no X/Y scaling.")
(princ)

; ============================================================
; V17 CORNER-CUT PANEL INSERTION OVERRIDES
;
; Changes:
; - Same block is used for every buildable panel:
;     FCD_Panel
; - Required dynamic properties:
;     L
;     H
;     LT_X LT_Y
;     RT_X RT_Y
;     RB_X RB_Y
;     LB_X LB_Y
; - RECT panels are inserted with all corner cuts = 0.
; - L_CUT panels are inserted with one active corner cut.
; - T_CUT panels are inserted with two active corner cuts.
; - U_CUT_RESERVED, SMALL, BIG and unsupported panels are skipped.
;
; Commands:
; - FCD_PANEL_BUILD       -> inserts RECT + L + T panels
; - FCD_PANEL_BUILD_RECT  -> alias to FCD_PANEL_BUILD for now
; ============================================================

(defun fcd:v17-zero-cuts ()
  (list
    (cons 'LT_X 0.0) (cons 'LT_Y 0.0)
    (cons 'RT_X 0.0) (cons 'RT_Y 0.0)
    (cons 'RB_X 0.0) (cons 'RB_Y 0.0)
    (cons 'LB_X 0.0) (cons 'LB_Y 0.0)
    (cons 'active 0)
    (cons 'valid T)
  )
)

(defun fcd:v17-put-cut-value (cuts key value)
  (if (assoc key cuts)
    (subst (cons key value) (assoc key cuts) cuts)
    (append cuts (list (cons key value)))
  )
)

(defun fcd:v17-get-cut-value (cuts key)
  (cdr (assoc key cuts))
)

(defun fcd:v17-cut-active-p (w h / tol)
  (setq tol (fcd:get 'tol))
  (and (> w tol) (> h tol))
)

(defun fcd:v17-component-bounds (comp / minI maxI minJ maxJ p i j)
  (setq minI nil)
  (setq maxI nil)
  (setq minJ nil)
  (setq maxJ nil)

  (foreach p comp
    (setq i (car p))
    (setq j (cadr p))

    (if (or (not minI) (< i minI)) (setq minI i))
    (if (or (not maxI) (> i maxI)) (setq maxI i))
    (if (or (not minJ) (< j minJ)) (setq minJ j))
    (if (or (not maxJ) (> j maxJ)) (setq maxJ j))
  )

  (if (and minI maxI minJ maxJ)
    (list minI minJ maxI maxJ)
    nil
  )
)

(defun fcd:v17-component-rectangular-p (comp / b minI minJ maxI maxJ expected)
  (setq b (fcd:v17-component-bounds comp))
  (if (not b)
    nil
    (progn
      (setq minI (nth 0 b))
      (setq minJ (nth 1 b))
      (setq maxI (nth 2 b))
      (setq maxJ (nth 3 b))
      (setq expected (* (1+ (- maxI minI)) (1+ (- maxJ minJ))))
      (= (length comp) expected)
    )
  )
)

(defun fcd:v17-add-coord-unique (v lst)
  (fcd:add-unique-number v lst (fcd:get 'tol))
)

(defun fcd:v17-poly-grid-coords (pts bbox / xs ys p)
  (setq xs nil)
  (setq ys nil)

  ; Always include bbox coordinates.
  (setq xs (fcd:v17-add-coord-unique (nth 0 bbox) xs))
  (setq xs (fcd:v17-add-coord-unique (nth 2 bbox) xs))
  (setq ys (fcd:v17-add-coord-unique (nth 1 bbox) ys))
  (setq ys (fcd:v17-add-coord-unique (nth 3 bbox) ys))

  ; Add all polygon vertex coordinates.
  (foreach p pts
    (setq xs (fcd:v17-add-coord-unique (car p) xs))
    (setq ys (fcd:v17-add-coord-unique (cadr p) ys))
  )

  (list
    (cons 'xs (fcd:sort-numbers xs))
    (cons 'ys (fcd:sort-numbers ys))
  )
)

(defun fcd:v17-missing-cells-in-bbox (pts bbox xs ys / missing i j x1 x2 y1 y2 cx cy tol)
  (setq missing nil)
  (setq tol (fcd:get 'tol))

  (setq i 0)
  (while (< i (1- (length xs)))
    (setq x1 (nth i xs))
    (setq x2 (nth (1+ i) xs))

    (setq j 0)
    (while (< j (1- (length ys)))
      (setq y1 (nth j ys))
      (setq y2 (nth (1+ j) ys))

      (setq cx (/ (+ x1 x2) 2.0))
      (setq cy (/ (+ y1 y2) 2.0))

      ; Cell is inside bbox by construction. If its center is not inside
      ; the real panel polygon, it is a cut/missing cell.
      (if (not (fcd:point-in-poly-p (list cx cy) pts tol))
        (setq missing (fcd:add-unique-int-pair (list i j) missing))
      )

      (setq j (1+ j))
    )

    (setq i (1+ i))
  )

  missing
)

(defun fcd:v17-component-containing (cell comps / hit c)
  (setq hit nil)
  (foreach c comps
    (if (and (not hit) (member cell c))
      (setq hit c)
    )
  )
  hit
)

(defun fcd:v17-component-id (comp comps / idx hit c)
  (setq idx 0)
  (setq hit nil)
  (foreach c comps
    (if (not hit)
      (progn
        (if (equal comp c)
          (setq hit idx)
        )
        (setq idx (1+ idx))
      )
    )
  )
  hit
)

(defun fcd:v17-component-corner-cut (corner comp xs ys bbox / b minI minJ maxI maxJ x1 y1 x2 y2 w h)
  (setq b (fcd:v17-component-bounds comp))
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))

  (if (not b)
    nil
    (progn
      (setq minI (nth 0 b))
      (setq minJ (nth 1 b))
      (setq maxI (nth 2 b))
      (setq maxJ (nth 3 b))

      (cond
        ((= corner 'LT)
          (setq w (- (nth (1+ maxI) xs) x1))
          (setq h (- y2 (nth minJ ys)))
        )
        ((= corner 'RT)
          (setq w (- x2 (nth minI xs)))
          (setq h (- y2 (nth minJ ys)))
        )
        ((= corner 'RB)
          (setq w (- x2 (nth minI xs)))
          (setq h (- (nth (1+ maxJ) ys) y1))
        )
        ((= corner 'LB)
          (setq w (- (nth (1+ maxI) xs) x1))
          (setq h (- (nth (1+ maxJ) ys) y1))
        )
      )

      (list w h)
    )
  )
)

(defun fcd:v17-apply-corner-cut (cuts corner wh / w h active)
  (if (not wh)
    (fcd:v17-put-cut-value cuts 'valid nil)
    (progn
      (setq w (car wh))
      (setq h (cadr wh))

      (if (not (fcd:v17-cut-active-p w h))
        cuts
        (progn
          (setq active (fcd:v17-get-cut-value cuts 'active))
          (if (not active) (setq active 0))

          (cond
            ((= corner 'LT)
              (setq cuts (fcd:v17-put-cut-value cuts 'LT_X w))
              (setq cuts (fcd:v17-put-cut-value cuts 'LT_Y h))
            )
            ((= corner 'RT)
              (setq cuts (fcd:v17-put-cut-value cuts 'RT_X w))
              (setq cuts (fcd:v17-put-cut-value cuts 'RT_Y h))
            )
            ((= corner 'RB)
              (setq cuts (fcd:v17-put-cut-value cuts 'RB_X w))
              (setq cuts (fcd:v17-put-cut-value cuts 'RB_Y h))
            )
            ((= corner 'LB)
              (setq cuts (fcd:v17-put-cut-value cuts 'LB_X w))
              (setq cuts (fcd:v17-put-cut-value cuts 'LB_Y h))
            )
          )

          (setq cuts (fcd:v17-put-cut-value cuts 'active (1+ active)))
          cuts
        )
      )
    )
  )
)

(defun fcd:v17-shape-corner-cuts (pts bbox cls / cuts coords xs ys missing comps maxI maxJ cornerCells corner comp compId usedIds wh valid active)
  (setq cuts (fcd:v17-zero-cuts))

  (cond
    ((= cls "RECT")
      cuts
    )

    ((or (= cls "L_CUT") (= cls "T_CUT"))
      (progn
        (setq coords (fcd:v17-poly-grid-coords pts bbox))
        (setq xs (cdr (assoc 'xs coords)))
        (setq ys (cdr (assoc 'ys coords)))
        (setq missing (fcd:v17-missing-cells-in-bbox pts bbox xs ys))
        (setq comps (fcd:index-components missing))

        (setq maxI (- (length xs) 2))
        (setq maxJ (- (length ys) 2))

        (setq cornerCells
          (list
            (list 'LT (list 0 maxJ))
            (list 'RT (list maxI maxJ))
            (list 'RB (list maxI 0))
            (list 'LB (list 0 0))
          )
        )

        (setq usedIds nil)

        (foreach cornerCellsItem cornerCells
          (setq corner (car cornerCellsItem))
          (setq comp (fcd:v17-component-containing (cadr cornerCellsItem) comps))

          (if comp
            (progn
              (setq compId (fcd:v17-component-id comp comps))

              ; One missing component touching multiple bbox corners is not a clean L/T
              ; corner-cut case. Do not build it automatically.
              (if (member compId usedIds)
                (setq cuts (fcd:v17-put-cut-value cuts 'valid nil))
                (progn
                  (setq usedIds (cons compId usedIds))

                  (if (not (fcd:v17-component-rectangular-p comp))
                    (setq cuts (fcd:v17-put-cut-value cuts 'valid nil))
                    (progn
                      (setq wh (fcd:v17-component-corner-cut corner comp xs ys bbox))
                      (setq cuts (fcd:v17-apply-corner-cut cuts corner wh))
                    )
                  )
                )
              )
            )
          )
        )

        (setq valid (cdr (assoc 'valid cuts)))
        (setq active (cdr (assoc 'active cuts)))

        (if (not valid)
          cuts
          (progn
            ; Guard against accidental mis-builds.
            ; L should have exactly one corner cut, T exactly two.
            (cond
              ((and (= cls "L_CUT") (/= active 1))
                (setq cuts (fcd:v17-put-cut-value cuts 'valid nil))
              )
              ((and (= cls "T_CUT") (/= active 2))
                (setq cuts (fcd:v17-put-cut-value cuts 'valid nil))
              )
            )
            cuts
          )
        )
      )
    )

    (T
      (fcd:v17-put-cut-value cuts 'valid nil)
    )
  )
)

(defun fcd:v17-set-dynprop-exact (obj propName value / res)
  (setq res (fcd:v16-set-dynprop-by-aliases obj (list propName) value))
  (and
    (cdr (assoc 'found res))
    (cdr (assoc 'set res))
  )
)

(defun fcd:v17-set-required-cut-props (obj cuts / ok names k v)
  (setq ok T)
  (setq names
    (list
      'LT_X 'LT_Y
      'RT_X 'RT_Y
      'RB_X 'RB_Y
      'LB_X 'LB_Y
    )
  )

  (foreach k names
    (setq v (cdr (assoc k cuts)))
    (if (not v) (setq v 0.0))

    (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name k) (* 1.0 v)))
      (setq ok nil)
    )
  )

  ok
)

(defun fcd:v17-set-panel-dynamic-geometry (obj width height cuts / sizeResult sizeOk cutOk)
  (setq sizeResult (fcd:v16-set-panel-dynamic-size obj width height))
  (setq sizeOk (cdr (assoc 'ok sizeResult)))

  ; Update after L/H first; some dynamic blocks need this before cut parameters
  ; evaluate correctly.
  (vl-catch-all-apply 'vla-Update (list obj))

  (setq cutOk (fcd:v17-set-required-cut-props obj cuts))
  (vl-catch-all-apply 'vla-Update (list obj))

  (and sizeOk cutOk)
)

(defun fcd:v17-insert-panel-shape (bbox cuts / x1 y1 x2 y2 w h blockName obj ok)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)
  (setq ok nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      (setq obj (fcd:v14-insert-raw blockName (list x1 y1 0.0) 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)

          (if (fcd:v17-set-panel-dynamic-geometry obj w h cuts)
            (progn
              (setq ok (fcd:v14-align-bbox-min-to-rect obj x1 y1))
              (vl-catch-all-apply 'vla-Update (list obj))
            )
          )

          (if ok
            obj
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v17-build-panel-blocks (shapes / inserted insertedRect insertedL insertedT skippedSmall skippedBig skippedU skippedUnsupported failed invalid failedCuts shape cls pts bbox x1 y1 x2 y2 w h maxW maxH cuts obj)
  (setq inserted 0)
  (setq insertedRect 0)
  (setq insertedL 0)
  (setq insertedT 0)
  (setq skippedSmall 0)
  (setq skippedBig 0)
  (setq skippedU 0)
  (setq skippedUnsupported 0)
  (setq failed 0)
  (setq failedCuts 0)
  (setq invalid 0)

  (setq maxW (fcd:get 'maxPanelWidth))
  (setq maxH (fcd:get 'maxPanelHeight))

  (foreach shape shapes
    (if (not shape)
      (setq invalid (1+ invalid))
      (progn
        (setq cls (cdr (assoc 'class shape)))
        (setq pts (cdr (assoc 'pts shape)))
        (setq bbox (cdr (assoc 'bbox shape)))

        (if (or (not pts) (not bbox))
          (setq invalid (1+ invalid))
          (progn
            (setq x1 (nth 0 bbox))
            (setq y1 (nth 1 bbox))
            (setq x2 (nth 2 bbox))
            (setq y2 (nth 3 bbox))
            (setq w (- x2 x1))
            (setq h (- y2 y1))

            (cond
              ((fcd:v12-shape-small-p pts bbox)
                (setq skippedSmall (1+ skippedSmall))
              )

              ((or (> w maxW) (> h maxH))
                (setq skippedBig (1+ skippedBig))
              )

              ((= cls "U_CUT_RESERVED")
                (setq skippedU (1+ skippedU))
              )

              ((not (or (= cls "RECT") (= cls "L_CUT") (= cls "T_CUT")))
                (setq skippedUnsupported (1+ skippedUnsupported))
              )

              (T
                (setq cuts (fcd:v17-shape-corner-cuts pts bbox cls))

                (if (not (cdr (assoc 'valid cuts)))
                  (setq failedCuts (1+ failedCuts))
                  (progn
                    (setq obj (fcd:v17-insert-panel-shape bbox cuts))

                    (if obj
                      (progn
                        (setq inserted (1+ inserted))
                        (cond
                          ((= cls "RECT") (setq insertedRect (1+ insertedRect)))
                          ((= cls "L_CUT") (setq insertedL (1+ insertedL)))
                          ((= cls "T_CUT") (setq insertedT (1+ insertedT)))
                        )
                      )
                      (setq failed (1+ failed))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (list
    (cons 'inserted inserted)
    (cons 'insertedRect insertedRect)
    (cons 'insertedL insertedL)
    (cons 'insertedT insertedT)
    (cons 'skippedSmall skippedSmall)
    (cons 'skippedBig skippedBig)
    (cons 'skippedU skippedU)
    (cons 'skippedUnsupported skippedUnsupported)
    (cons 'failedCuts failedCuts)
    (cons 'failed failed)
    (cons 'invalid invalid)
  )
)

(defun fcd:v17-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. Будут вставлены RECT/L/T области блоком "
          blockName
          " через динамические свойства L/H и угловые вырезы LT/RT/RB/LB."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V17. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (if (> (cdr (assoc 'failed stats)) 0)
                (fcd:msg "Если есть ошибки вставки/динамики, проверь в FCD_Panel редактируемые параметры: L, H, LT_X, LT_Y, RT_X, RT_Y, RB_X, RB_Y, LB_X, LB_Y.")
              )

              (if (> (cdr (assoc 'failedCuts stats)) 0)
                (fcd:msg "Если есть ошибки расчета вырезов, форма классифицирована как L/T, но ее отсутствующие зоны не похожи на чистые угловые прямоугольники.")
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v17-panel-build-command)
)

; Backward-compatible alias: the old command name now builds RECT/L/T.
(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v17-panel-build-command)
)

(princ "\nFCD_CORE_V17 loaded: FCD_Panel insertion supports RECT, L_CUT and T_CUT through corner dynamic parameters.")
(princ)


; ============================================================
; V18 DYNAMIC CORNER PARAMETER FIX
;
; Problem found in real FCD_Panel block:
; - Corner cut dynamic properties are not true "0 = no cut" sizes.
; - They are grip/parameter coordinates inside the dynamic block and their
;   neutral values may be non-zero after L/H evaluation.
;
; Fix:
; - Do NOT write inactive corner properties anymore.
; - For each newly inserted block, set L/H first and let the block evaluate.
; - Then write ONLY the active corner parameters required by L/T shapes.
; - RECT therefore keeps the block's own neutral/default corner values.
;
; Added command:
; - FCD_DUMP_DYNPROPS: select one dynamic block and print property names/values.
; ============================================================

(defun fcd:v18-cut-base ()
  (list
    (cons 'active 0)
    (cons 'valid T)
  )
)

; Override V17: no inactive cuts are included in the alist.
(defun fcd:v17-zero-cuts ()
  (fcd:v18-cut-base)
)

(defun fcd:v18-cut-keys ()
  (list 'LT_X 'LT_Y 'RT_X 'RT_Y 'RB_X 'RB_Y 'LB_X 'LB_Y)
)

(defun fcd:v18-cut-key-present-p (cuts key)
  (if (assoc key cuts) T nil)
)

(defun fcd:v18-dynprop-value (prop / res)
  (setq res (vl-catch-all-apply 'vlax-get-property (list prop 'Value)))
  (if (vl-catch-all-error-p res)
    nil
    res
  )
)

(defun fcd:v18-dump-dynprops-of-object (obj / props prop pname pval line)
  (setq props (fcd:v16-get-dynamic-properties obj))

  (if (not props)
    (fcd:msg "У выбранного объекта не удалось прочитать dynamic properties.")
    (progn
      (fcd:msg "Dynamic properties выбранного блока:")
      (foreach prop props
        (setq pname (fcd:v16-dynprop-name prop))
        (setq pval (fcd:v18-dynprop-value prop))
        (setq line
          (strcat
            "  "
            (if pname pname "<no name>")
            " = "
            (cond
              ((numberp pval) (rtos pval 2 4))
              ((= (type pval) 'STR) pval)
              ((null pval) "<nil>")
              (T (vl-princ-to-string pval))
            )
          )
        )
        (princ (strcat "\n" line))
      )
    )
  )
)

(defun c:FCD_DUMP_DYNPROPS (/ ent obj)
  (setq ent (car (entsel "\nВыбери вставленный FCD_Panel для вывода dynamic properties: ")))
  (if (not ent)
    (fcd:msg "Объект не выбран.")
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (fcd:v18-dump-dynprops-of-object obj)
    )
  )
  (princ)
)

; Override V17: set ONLY active cut parameters. Missing keys are intentionally
; skipped so the block keeps its own neutral/default coordinate values.
(defun fcd:v17-set-required-cut-props (obj cuts / ok names k v)
  (setq ok T)
  (setq names (fcd:v18-cut-keys))

  (foreach k names
    (if (fcd:v18-cut-key-present-p cuts k)
      (progn
        (setq v (cdr (assoc k cuts)))
        (if (not (numberp v))
          (setq ok nil)
          (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name k) (* 1.0 v)))
            (setq ok nil)
          )
        )
      )
    )
  )

  ok
)

; Override V17: L/H first, update, then only active cut handles.
(defun fcd:v17-set-panel-dynamic-geometry (obj width height cuts / sizeResult sizeOk cutOk)
  (setq sizeResult (fcd:v16-set-panel-dynamic-size obj width height))
  (setq sizeOk (cdr (assoc 'ok sizeResult)))

  ; Let FCD_Panel recompute internal neutral corner coordinates after L/H.
  (vl-catch-all-apply 'vla-Update (list obj))

  (setq cutOk (fcd:v17-set-required-cut-props obj cuts))
  (vl-catch-all-apply 'vla-Update (list obj))

  (and sizeOk cutOk)
)

; More explicit build command message for V18.
(defun fcd:v18-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V18: RECT/L/T вставляются блоком "
          blockName
          "; L/H задаются всегда, а угловые LT/RT/RB/LB параметры пишутся только для активных вырезов."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V18. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (if (> (cdr (assoc 'failed stats)) 0)
                (fcd:msg "Если есть ошибки вставки/динамики, проверь параметры FCD_Panel: L, H, LT_X, LT_Y, RT_X, RT_Y, RB_X, RB_Y, LB_X, LB_Y. Для диагностики используй FCD_DUMP_DYNPROPS.")
              )

              (if (> (cdr (assoc 'failedCuts stats)) 0)
                (fcd:msg "Если есть ошибки расчета вырезов, форма классифицирована как L/T, но отсутствующие зоны не похожи на чистые угловые прямоугольники.")
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v18-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v18-panel-build-command)
)

(princ "\nFCD_CORE_V18 loaded: inactive corner cut dynamic properties are preserved; only active cuts are written.")
(princ)

; ============================================================
; V19 CORNER HANDLE COORDINATE MODE
;
; Real FCD_Panel block behavior found from testing:
; - Corner dynamic properties are grip/handle coordinates, not pure cut sizes.
; - For right-side corners, X must be written as the local X coordinate of the
;   inner vertical cut edge, not as distance from the right side.
;
; Therefore:
;   LT_X = inner cut X from left side
;   RT_X = inner cut X from left side
;   RB_X = inner cut X from left side
;   LB_X = inner cut X from left side
;
;   LT_Y / RT_Y = cut depth from top side
;   LB_Y / RB_Y = cut height from bottom side
;
; This overrides V17 component-to-corner calculation while keeping V18 behavior:
; inactive properties are not touched.
; ============================================================

(defun fcd:v19-num-from-variant (v / val)
  (cond
    ((numberp v) v)
    ((= (type v) 'VARIANT)
      (setq val (vlax-variant-value v))
      (cond
        ((numberp val) val)
        ((= (type val) 'SAFEARRAY) (vlax-safearray->list val))
        (T val)
      )
    )
    ((= (type v) 'SAFEARRAY) (vlax-safearray->list v))
    (T v)
  )
)

; Override V18 dump conversion so numbers are printed as numbers, not #<variant ...>.
(defun fcd:v18-dynprop-value (prop / res)
  (setq res (vl-catch-all-apply 'vlax-get-property (list prop 'Value)))
  (if (vl-catch-all-error-p res)
    nil
    (fcd:v19-num-from-variant res)
  )
)

; Override V17: return target dynamic handle values, not raw cut widths.
(defun fcd:v17-component-corner-cut (corner comp xs ys bbox / b minI minJ maxI maxJ x1 y1 x2 y2 xVal yVal)
  (setq b (fcd:v17-component-bounds comp))
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))

  (if (not b)
    nil
    (progn
      (setq minI (nth 0 b))
      (setq minJ (nth 1 b))
      (setq maxI (nth 2 b))
      (setq maxJ (nth 3 b))

      (cond
        ; Left-top cut. Inner vertical edge is the right edge of the missing block.
        ; Y is depth down from the top side.
        ((= corner 'LT)
          (setq xVal (- (nth (1+ maxI) xs) x1))
          (setq yVal (- y2 (nth minJ ys)))
        )

        ; Right-top cut. Inner vertical edge is the left edge of the missing block.
        ; X is local coordinate from the left side, not cut width from the right.
        ; Y is depth down from the top side.
        ((= corner 'RT)
          (setq xVal (- (nth minI xs) x1))
          (setq yVal (- y2 (nth minJ ys)))
        )

        ; Right-bottom cut. Inner vertical edge is the left edge of the missing block.
        ; X is local coordinate from the left side, not cut width from the right.
        ; Y is height up from the bottom side.
        ((= corner 'RB)
          (setq xVal (- (nth minI xs) x1))
          (setq yVal (- (nth (1+ maxJ) ys) y1))
        )

        ; Left-bottom cut. Inner vertical edge is the right edge of the missing block.
        ; Y is height up from the bottom side.
        ((= corner 'LB)
          (setq xVal (- (nth (1+ maxI) xs) x1))
          (setq yVal (- (nth (1+ maxJ) ys) y1))
        )
      )

      (list xVal yVal)
    )
  )
)

; More explicit message for V19.
(defun fcd:v19-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V19: RECT/L/T вставляются блоком "
          blockName
          "; угловые параметры пишутся как координаты ручек вырезов, не как размеры от углов."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V19. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v19-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v19-panel-build-command)
)

(princ "\nFCD_CORE_V19 loaded: corner cut parameters use block handle coordinate mode.")
(princ)

; ============================================================
; V20 INSERTION POINT / POSITIVE CUT VALUE MODE
;
; Real block behavior after testing:
; - FCD_Panel corner parameters are positive dynamic handle values.
; - BBox alignment after setting L/H/cuts is harmful for cut blocks, because
;   the visible bbox changes with cuts and AutoCAD shifts the internal dynamic
;   origins. This is why the same visual panel could have different handle
;   values after manual correction.
;
; New convention:
; - The FCD_Panel block insertion/base point is the stable control point of the
;   panel, preferably the center of its full L x H frame.
; - LISP inserts FCD_Panel directly at the center of the found panel bbox.
; - LISP sets L/H and active cut handles.
; - LISP DOES NOT realign by GetBoundingBox afterwards.
;
; This preserves the dynamic origins and keeps all corner cut values positive.
; ============================================================

(defun fcd:v20-bbox-center (bbox / x1 y1 x2 y2)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (list (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0) 0.0)
)

; Override V17/V19 insertion. Do not align by visible bbox.
(defun fcd:v17-insert-panel-shape (bbox cuts / x1 y1 x2 y2 w h blockName obj ok insPt)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)
  (setq ok nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      ; FCD_Panel base point must be the stable control point of the block.
      ; For the current block convention this is the center of the full panel frame.
      (setq insPt (fcd:v20-bbox-center bbox))
      (setq obj (fcd:v14-insert-raw blockName insPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)

          (if (fcd:v17-set-panel-dynamic-geometry obj w h cuts)
            (progn
              ; Important: no bbox alignment here. Visible bbox is cut-dependent
              ; and moving it back breaks dynamic handle origins.
              (vl-catch-all-apply 'vla-Update (list obj))
              (setq ok T)
            )
          )

          (if ok
            obj
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v20-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V20: RECT/L/T вставляются блоком "
          blockName
          "; вставка идет в центр полного LxH-габарита, без bbox-выравнивания после вырезов."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V20. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (if (> (cdr (assoc 'failed stats)) 0)
                (fcd:msg "Если есть ошибки вставки/динамики, проверь параметры FCD_Panel: L, H, LT_X, LT_Y, RT_X, RT_Y, RB_X, RB_Y, LB_X, LB_Y. Для диагностики используй FCD_DUMP_DYNPROPS.")
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v20-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v20-panel-build-command)
)

(princ "\nFCD_CORE_V20 loaded: FCD_Panel inserted by stable center point; no post-cut bbox alignment.")
(princ)

; ============================================================
; V21 FULL-FRAME ALIGN BEFORE CUTS
;
; Correction after real FCD_Panel block testing:
; - Inserting at the geometric center of L x H is not reliable when corner cuts
;   can be larger than half of the panel dimension.
; - Visible-bbox alignment AFTER cuts is also wrong, because the visible bbox is
;   already affected by the cut geometry.
;
; New build order:
;   1) Insert FCD_Panel at a neutral temporary point.
;   2) Set only L/H.
;   3) Update the block.
;   4) Align the block's FULL RECTANGLE bbox to the target panel bbox.
;      At this moment no active cuts have been written yet, so bbox represents
;      the complete L x H frame, not the cut visible shape.
;   5) Write only active corner cut parameters.
;   6) Update again.
;
; This keeps dynamic origins stable and avoids the center-point assumption.
; ============================================================

(defun fcd:v21-set-panel-lh-only (obj width height / sizeResult sizeOk)
  (setq sizeResult (fcd:v16-set-panel-dynamic-size obj width height))
  (setq sizeOk (cdr (assoc 'ok sizeResult)))
  (vl-catch-all-apply 'vla-Update (list obj))
  sizeOk
)

(defun fcd:v21-set-cuts-only (obj cuts / cutOk)
  (setq cutOk (fcd:v17-set-required-cut-props obj cuts))
  (vl-catch-all-apply 'vla-Update (list obj))
  cutOk
)

; Override V17/V20 insertion again.
; The insertion point is deliberately not semantically important anymore.
; Alignment is made after L/H and before active cuts.
(defun fcd:v17-insert-panel-shape (bbox cuts / x1 y1 x2 y2 w h blockName obj ok tempPt lhOk alignOk cutOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)
  (setq ok nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      ; Put it roughly near the panel. Exact location is corrected by full-frame
      ; bbox alignment after L/H.
      (setq tempPt (list x1 y1 0.0))
      (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)

          ; 1) Set only full frame size.
          (setq lhOk (fcd:v21-set-panel-lh-only obj w h))

          ; 2) Align by full uncut bbox, not by cut bbox.
          (setq alignOk nil)
          (if lhOk
            (setq alignOk (fcd:v14-align-bbox-min-to-rect obj x1 y1))
          )

          ; 3) Now apply active cuts only. No movement after this.
          (setq cutOk nil)
          (if alignOk
            (setq cutOk (fcd:v21-set-cuts-only obj cuts))
          )

          (if (and lhOk alignOk cutOk)
            (progn
              (vl-catch-all-apply 'vla-Update (list obj))
              (setq ok T)
            )
          )

          (if ok
            obj
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v21-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V21: RECT/L/T вставляются блоком "
          blockName
          "; порядок: L/H -> выравнивание полного bbox -> активные вырезы."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V21. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (if (> (cdr (assoc 'failed stats)) 0)
                (fcd:msg "Если есть ошибки вставки/динамики, проверь параметры FCD_Panel: L, H, LT_X, LT_Y, RT_X, RT_Y, RB_X, RB_Y, LB_X, LB_Y. Для диагностики используй FCD_DUMP_DYNPROPS.")
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v21-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v21-panel-build-command)
)

(princ "\nFCD_CORE_V21 loaded: full-frame bbox is aligned after L/H and before corner cuts.")
(princ)

; ============================================================
; V22 POSITIVE HANDLE OFFSET MODE
;
; Real block behavior after testing:
; - LT_X/LT_Y/... are positive grip/handle values.
; - A no-cut state is not value 0. It is the local Origin offset of the
;   dynamic parameter.
; - For active cuts, the required value is:
;       dynamic Origin local offset + calculated cut coordinate/depth
;
; Build order:
;   1) Insert FCD_Panel.
;   2) Reset dynamic block if AutoCAD supports ResetBlock.
;   3) Set L/H.
;   4) Neutralize all Y cut handles using their Origin offsets.
;   5) Align full uncut bbox to target rectangle.
;   6) Re-read Origin offsets after the move.
;   7) Set active corner X/Y handles as Origin offset + calculated value.
;   8) Do not move the block after cuts.
;
; This fixes:
; - RECT panels keeping old/default cut handles.
; - cut blocks flying away after bbox-align.
; - negative/invalid cut values when the block requires positive handles.
; ============================================================

(defun fcd:v22-point3 (v / p)
  (setq p (fcd:v19-num-from-variant v))
  (cond
    ((and (listp p) (>= (length p) 2))
      (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))
    )
    (T nil)
  )
)

(defun fcd:v22-block-insertion-point (obj / res p)
  (setq res (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p res)
    (list 0.0 0.0 0.0)
    (progn
      (setq p (fcd:v22-point3 res))
      (if p p (list 0.0 0.0 0.0))
    )
  )
)

(defun fcd:v22-origin-name-p (name)
  (and name (= (strcase name) "ORIGIN"))
)

(defun fcd:v22-find-dynprop-with-origin (obj propName / props prop pname target hit wantOrigin originVal)
  (setq props (fcd:v16-get-dynamic-properties obj))
  (setq target (strcase propName))
  (setq hit nil)
  (setq wantOrigin nil)
  (setq originVal nil)

  (if props
    (foreach prop props
      (setq pname (fcd:v16-dynprop-name prop))

      (cond
        (wantOrigin
          (progn
            (if (fcd:v22-origin-name-p pname)
              (setq originVal (fcd:v22-point3 (fcd:v18-dynprop-value prop)))
            )
            (setq wantOrigin nil)
          )
        )

        ((and
           (not hit)
           pname
           (= (strcase pname) target)
         )
          (progn
            (setq hit prop)
            ; In this block AutoCAD exposes a dynamic property named Origin
            ; immediately after the real distance/position property.
            (setq wantOrigin T)
          )
        )
      )
    )
  )

  (list
    (cons 'prop hit)
    (cons 'origin originVal)
  )
)

(defun fcd:v22-axis-of-cut-key (key)
  (if (wcmatch (strcase (vl-symbol-name key)) "*_X")
    'X
    'Y
  )
)

(defun fcd:v22-origin-local-offset (obj key / info origin ins axis val)
  (setq info (fcd:v22-find-dynprop-with-origin obj (vl-symbol-name key)))
  (setq origin (cdr (assoc 'origin info)))
  (setq ins (fcd:v22-block-insertion-point obj))
  (setq axis (fcd:v22-axis-of-cut-key key))

  (cond
    ((and origin (= axis 'X))
      (setq val (- (car origin) (car ins)))
    )
    ((and origin (= axis 'Y))
      (setq val (- (cadr origin) (cadr ins)))
    )
    (T
      (setq val 0.0)
    )
  )

  val
)

(defun fcd:v22-set-cut-key-value (obj key localValue / info prop originLocal targetValue res)
  (setq info (fcd:v22-find-dynprop-with-origin obj (vl-symbol-name key)))
  (setq prop (cdr (assoc 'prop info)))

  (if (not prop)
    nil
    (progn
      (setq originLocal (fcd:v22-origin-local-offset obj key))
      (setq targetValue (+ originLocal localValue))
      (setq res (vl-catch-all-apply 'vlax-put-property (list prop 'Value (* 1.0 targetValue))))
      (not (vl-catch-all-error-p res))
    )
  )
)

(defun fcd:v22-neutral-y-keys ()
  (list 'LT_Y 'RT_Y 'RB_Y 'LB_Y)
)

(defun fcd:v22-x-keys ()
  (list 'LT_X 'RT_X 'RB_X 'LB_X)
)

(defun fcd:v22-neutralize-cut-depths (obj / ok key)
  (setq ok T)

  ; Only Y/depth handles are required to suppress visible cuts.
  ; X handles are irrelevant when depth is neutral and leaving them alone
  ; avoids unnecessary jumps of hidden construction geometry.
  (foreach key (fcd:v22-neutral-y-keys)
    (if (not (fcd:v22-set-cut-key-value obj key 0.0))
      (setq ok nil)
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v22-apply-active-cuts (obj cuts / ok key val)
  (setq ok T)

  ; Start from neutral depths every time so RECT and inactive corners do not
  ; inherit whatever state the block definition had when it was saved.
  (if (not (fcd:v22-neutralize-cut-depths obj))
    (setq ok nil)
  )

  ; X first, then Y/depth. With the positive-handle block, the cut becomes
  ; visible only after the corresponding Y handle leaves its neutral position.
  (foreach key (fcd:v22-x-keys)
    (if (assoc key cuts)
      (progn
        (setq val (cdr (assoc key cuts)))
        (if (not (fcd:v22-set-cut-key-value obj key (* 1.0 val)))
          (setq ok nil)
        )
      )
    )
  )

  (foreach key (fcd:v22-neutral-y-keys)
    (if (assoc key cuts)
      (progn
        (setq val (cdr (assoc key cuts)))
        (if (not (fcd:v22-set-cut-key-value obj key (* 1.0 val)))
          (setq ok nil)
        )
      )
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v22-reset-block-if-possible (obj)
  ; Some AutoCAD versions expose ResetBlock for dynamic references.
  ; It is safe to try and ignore failure.
  (vl-catch-all-apply 'vla-ResetBlock (list obj))
  (vl-catch-all-apply 'vla-Update (list obj))
  T
)

; Override V21 insertion.
(defun fcd:v17-insert-panel-shape (bbox cuts / x1 y1 x2 y2 w h blockName obj ok tempPt lhOk neutralOk alignOk cutOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)
  (setq ok nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      (setq tempPt (list x1 y1 0.0))
      (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)
          (fcd:v22-reset-block-if-possible obj)

          ; Full frame first.
          (setq lhOk (fcd:v21-set-panel-lh-only obj w h))

          ; Suppress default/saved cuts before full-bbox alignment.
          (setq neutralOk nil)
          (if lhOk
            (setq neutralOk (fcd:v22-neutralize-cut-depths obj))
          )

          ; Now bbox should represent the full uncut L x H frame.
          (setq alignOk nil)
          (if neutralOk
            (setq alignOk (fcd:v14-align-bbox-min-to-rect obj x1 y1))
          )

          ; After moving the block, re-read Origin offsets and apply final cuts.
          (setq cutOk nil)
          (if alignOk
            (setq cutOk (fcd:v22-apply-active-cuts obj cuts))
          )

          (if (and lhOk neutralOk alignOk cutOk)
            (progn
              (vl-catch-all-apply 'vla-Update (list obj))
              (setq ok T)
            )
          )

          (if ok
            obj
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v22-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V22: RECT/L/T вставляются блоком "
          blockName
          "; вырезы задаются как положительные значения Origin+координата/глубина."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V22. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (if (> (cdr (assoc 'failed stats)) 0)
                (fcd:msg "Если есть ошибки вставки/динамики, используй FCD_DUMP_DYNPROPS на одном авто-блоке и одном вручную исправленном блоке.")
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v22-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v22-panel-build-command)
)

(princ "\nFCD_CORE_V22 loaded: positive corner handles use Origin-offset mode; RECT cuts are neutralized.")
(princ)

; ============================================================
; V23 SAFE ACTIVE-CUT MODE
;
; V22 was too aggressive: it tried to neutralize cut handles through Origin
; values and many FCD_Panel blocks reject those values as invalid. Result:
; all panel insertions could fail and get deleted.
;
; V23 reverts to the stable build pipeline:
;   1) Insert FCD_Panel.
;   2) Reset dynamic reference if possible.
;   3) Set L/H only.
;   4) Align by current full/default bbox.
;   5) Write ONLY active corner parameters for L/T shapes.
;   6) Do not write inactive LT/RT/RB/LB values at all.
;   7) Do not move after cuts.
;
; Requirement for the block definition:
;   The saved/default state of FCD_Panel after setting L/H must be RECT
;   without visible cuts. RECT panels do not touch corner properties.
; ============================================================

(defun fcd:v23-cut-param-keys ()
  (list 'LT_X 'LT_Y 'RT_X 'RT_Y 'RB_X 'RB_Y 'LB_X 'LB_Y)
)

(defun fcd:v23-active-cut-pair-p (pair)
  (and
    pair
    (member (car pair) (fcd:v23-cut-param-keys))
    (numberp (cdr pair))
    (> (abs (cdr pair)) (fcd:get 'tol))
  )
)

(defun fcd:v23-set-active-cut-props (obj cuts / ok pair key val)
  (setq ok T)

  ; Important: do NOT write inactive corners. Their neutral value is block-defined
  ; and may be non-zero / positive-only / dependent on L/H.
  (foreach pair cuts
    (if (fcd:v23-active-cut-pair-p pair)
      (progn
        (setq key (car pair))
        (setq val (cdr pair))
        (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name key) (* 1.0 val)))
          (setq ok nil)
        )
      )
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v23-insert-panel-shape-safe (bbox cuts / x1 y1 x2 y2 w h blockName obj tempPt lhOk alignOk cutOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      (setq tempPt (list x1 y1 0.0))
      (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)
          (fcd:v22-reset-block-if-possible obj)

          ; Full size first.
          (setq lhOk (fcd:v21-set-panel-lh-only obj w h))

          ; Align while only default/RECT state is active.
          (setq alignOk nil)
          (if lhOk
            (setq alignOk (fcd:v14-align-bbox-min-to-rect obj x1 y1))
          )

          ; Active L/T cuts only. RECT writes nothing here.
          (setq cutOk T)
          (if alignOk
            (setq cutOk (fcd:v23-set-active-cut-props obj cuts))
          )

          ; Do not delete the object only because cut props failed: keeping it
          ; visible makes debugging possible. Delete only if L/H or base align failed.
          (cond
            ((and lhOk alignOk)
              (vl-catch-all-apply 'vla-Update (list obj))
              obj
            )
            (T
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

; Override the insertion hook used by fcd:v17-build-panel-blocks.
(defun fcd:v17-insert-panel-shape (bbox cuts)
  (fcd:v23-insert-panel-shape-safe bbox cuts)
)

(defun fcd:v23-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V23: RECT/L/T вставляются блоком "
          blockName
          "; L/H -> выравнивание -> запись только активных углов."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V23. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (fcd:msg "Примечание V23: RECT не трогает LT/RT/RB/LB. Сохрани FCD_Panel в дефолтном состоянии без вырезов.")
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v23-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v23-panel-build-command)
)

(princ "\nFCD_CORE_V23 loaded: safe active-cut mode; inactive cut handles are not written.")
(princ)

; ============================================================
; V24 CROSS-CUT HANDLE VALUE MODE
;
; Real FCD_Panel cross-cut block calibration from manual 4000 x 4000 test:
; Desired corner cut = 1000 x 1000 produced:
;   LT_X / LB_X = 1000 + X_OFFSET
;   RT_X / RB_X = 1000 - X_OFFSET
;   LT_Y / RT_Y = 1000 - Y_OFFSET
;   LB_Y / RB_Y = 1000 + Y_OFFSET
;
; Where X_OFFSET is the X component of the corresponding X dynamic
; parameter Origin (usually about 30.0211), and Y_OFFSET is the Y component
; of the corresponding Y dynamic parameter Origin (usually about 37.9313).
;
; Important: FCD_Panel corner parameters are NOT coordinates from the full
; panel frame. They are positive handle values around the dynamic parameter
; Origin. Therefore V19 coordinate mode is overridden here.
;
; V24 behavior:
; - L/H are still set normally.
; - L/T geometric missing corner sizes are calculated as true cut sizes.
; - Active corner parameters are written using the calibrated handle formula.
; - Inactive corner parameters are not touched; save FCD_Panel default as RECT
;   if you need clean rectangular panels.
; ============================================================

(defun fcd:v24-absnum (x)
  (cond
    ((numberp x) (abs x))
    (T 0.0)
  )
)

(defun fcd:v24-cut-key-side-x (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (cond
    ((or (= s "LT_X") (= s "LB_X")) 'LEFT)
    ((or (= s "RT_X") (= s "RB_X")) 'RIGHT)
    (T nil)
  )
)

(defun fcd:v24-cut-key-side-y (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (cond
    ((or (= s "LT_Y") (= s "RT_Y")) 'TOP)
    ((or (= s "LB_Y") (= s "RB_Y")) 'BOTTOM)
    (T nil)
  )
)

(defun fcd:v24-cut-key-axis (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (if (wcmatch s "*_X") 'X 'Y)
)

(defun fcd:v24-prop-origin-axis-offset (obj key / info origin axis)
  (setq info (fcd:v22-find-dynprop-with-origin obj (vl-symbol-name key)))
  (setq origin (cdr (assoc 'origin info)))
  (setq axis (fcd:v24-cut-key-axis key))

  ; The Origin values reported by AutoCAD for dynamic parameters are the
  ; reliable calibration source for this block. For X-parameters use Origin.X,
  ; for Y-parameters use Origin.Y. Do NOT subtract block insertion point here:
  ; the tested block exposes these as block-parameter coordinates.
  (cond
    ((and origin (= axis 'X)) (fcd:v24-absnum (car origin)))
    ((and origin (= axis 'Y)) (fcd:v24-absnum (cadr origin)))
    (T 0.0)
  )
)

(defun fcd:v24-target-cut-handle-value (obj key cutSize / axis side off v)
  (setq axis (fcd:v24-cut-key-axis key))
  (setq off (fcd:v24-prop-origin-axis-offset obj key))

  (cond
    ((= axis 'X)
      (setq side (fcd:v24-cut-key-side-x key))
      (cond
        ((= side 'LEFT)  (setq v (+ cutSize off)))
        ((= side 'RIGHT) (setq v (- cutSize off)))
        (T               (setq v cutSize))
      )
    )

    ((= axis 'Y)
      (setq side (fcd:v24-cut-key-side-y key))
      (cond
        ((= side 'TOP)    (setq v (- cutSize off)))
        ((= side 'BOTTOM) (setq v (+ cutSize off)))
        (T                (setq v cutSize))
      )
    )

    (T
      (setq v cutSize)
    )
  )

  v
)

; Override V19: shape analysis must return TRUE geometric corner cut sizes,
; not block-handle coordinates. The handle conversion is now done only when
; writing dynamic properties.
(defun fcd:v17-component-corner-cut (corner comp xs ys bbox / b minI minJ maxI maxJ x1 y1 x2 y2 w h)
  (setq b (fcd:v17-component-bounds comp))
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))

  (if (not b)
    nil
    (progn
      (setq minI (nth 0 b))
      (setq minJ (nth 1 b))
      (setq maxI (nth 2 b))
      (setq maxJ (nth 3 b))

      (cond
        ((= corner 'LT)
          (setq w (- (nth (1+ maxI) xs) x1))
          (setq h (- y2 (nth minJ ys)))
        )
        ((= corner 'RT)
          (setq w (- x2 (nth minI xs)))
          (setq h (- y2 (nth minJ ys)))
        )
        ((= corner 'RB)
          (setq w (- x2 (nth minI xs)))
          (setq h (- (nth (1+ maxJ) ys) y1))
        )
        ((= corner 'LB)
          (setq w (- (nth (1+ maxI) xs) x1))
          (setq h (- (nth (1+ maxJ) ys) y1))
        )
      )

      (list w h)
    )
  )
)

; Override V23: write active cut props through the calibrated cross-block
; handle formula.
(defun fcd:v23-set-active-cut-props (obj cuts / ok pair key cutSize handleValue)
  (setq ok T)

  ; Important: do NOT write inactive corners. Their neutral value is block-defined.
  (foreach pair cuts
    (if (fcd:v23-active-cut-pair-p pair)
      (progn
        (setq key (car pair))
        (setq cutSize (cdr pair))
        (setq handleValue (fcd:v24-target-cut-handle-value obj key (* 1.0 cutSize)))

        (if (not (numberp handleValue))
          (setq ok nil)
          (progn
            ; If the block rejects very small positive-handle values, fail this
            ; cut cleanly instead of sending it far away.
            (if (< handleValue 0.0)
              (setq ok nil)
              (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name key) (* 1.0 handleValue)))
                (setq ok nil)
              )
            )
          )
        )
      )
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v24-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V24: RECT/L/T вставляются блоком "
          blockName
          "; угловые вырезы пишутся по формуле крестового блока: LEFT/BOTTOM + Origin offset, RIGHT/TOP - Origin offset."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V24. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (fcd:msg "Примечание V24: RECT не трогает LT/RT/RB/LB. Дефолт FCD_Panel лучше сохранить без видимых вырезов.")
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v24-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v24-panel-build-command)
)

(princ "\nFCD_CORE_V24 loaded: cross-block corner cut handles use Origin-calibrated values.")
(princ)

; ============================================================
; V25 CROSS-BLOCK TRUE CUT MODEL
;
; Analysis of the uploaded drawing showed the previous formula was wrong.
; In FCD_Panel the corner parameters are dynamic handle distances from their
; parameter origins to the INNER cut lines, not raw cut sizes.
;
; For a panel with full size L x H and a real corner cut size Cx/Cy:
;
;   LEFT  X handles: value = (L / 2 - Cx) + Origin.X
;   RIGHT X handles: value = (L / 2 - Cx) - Origin.X
;
;   TOP    Y handles: value = (H / 2 - Cy) - Origin.Y
;   BOTTOM Y handles: value = (H / 2 - Cy) + Origin.Y
;
; Therefore a RECT panel is NOT "do not touch cuts". RECT means Cx/Cy = 0
; for all four corners, so every corner parameter must be written to its
; neutral full-frame value. This fixes rectangular panels that stayed with
; clipped corners and also makes L/T panels deterministic.
; ============================================================

(defun fcd:v25-number-value (v / inner)
  (cond
    ((numberp v) v)
    ((= (type v) 'VARIANT)
      (setq inner (vlax-variant-value v))
      (if (numberp inner) inner nil)
    )
    (T nil)
  )
)

(defun fcd:v25-get-dynprop-number (obj propName / props prop pname val hit)
  (setq props (fcd:v16-get-dynamic-properties obj))
  (setq hit nil)

  (if props
    (foreach prop props
      (if (not hit)
        (progn
          (setq pname (fcd:v16-dynprop-name prop))
          (if (and pname (= (strcase pname) (strcase propName)))
            (progn
              (setq val (fcd:v18-dynprop-value prop))
              (setq hit (fcd:v25-number-value val))
            )
          )
        )
      )
    )
  )

  hit
)

(defun fcd:v25-cut-key-side-x (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (cond
    ((or (= s "LT_X") (= s "LB_X")) 'LEFT)
    ((or (= s "RT_X") (= s "RB_X")) 'RIGHT)
    (T nil)
  )
)

(defun fcd:v25-cut-key-side-y (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (cond
    ((or (= s "LT_Y") (= s "RT_Y")) 'TOP)
    ((or (= s "LB_Y") (= s "RB_Y")) 'BOTTOM)
    (T nil)
  )
)

(defun fcd:v25-cut-key-axis (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (if (wcmatch s "*_X") 'X 'Y)
)

(defun fcd:v25-origin-axis-offset (obj key / info origin axis)
  (setq info (fcd:v22-find-dynprop-with-origin obj (vl-symbol-name key)))
  (setq origin (cdr (assoc 'origin info)))
  (setq axis (fcd:v25-cut-key-axis key))

  (cond
    ((and origin (= axis 'X) (numberp (car origin))) (abs (car origin)))
    ((and origin (= axis 'Y) (numberp (cadr origin))) (abs (cadr origin)))
    (T 0.0)
  )
)

(defun fcd:v25-target-corner-handle-value (obj key cutSize panelW panelH / axis side off halfFree val minVal)
  (setq axis (fcd:v25-cut-key-axis key))
  (setq off (fcd:v25-origin-axis-offset obj key))
  (setq minVal 0.001)

  (cond
    ((= axis 'X)
      (setq side (fcd:v25-cut-key-side-x key))
      (setq halfFree (- (/ panelW 2.0) cutSize))
      (cond
        ((= side 'LEFT)  (setq val (+ halfFree off)))
        ((= side 'RIGHT) (setq val (- halfFree off)))
        (T               (setq val halfFree))
      )
    )

    ((= axis 'Y)
      (setq side (fcd:v25-cut-key-side-y key))
      (setq halfFree (- (/ panelH 2.0) cutSize))
      (cond
        ((= side 'TOP)    (setq val (- halfFree off)))
        ((= side 'BOTTOM) (setq val (+ halfFree off)))
        (T                (setq val halfFree))
      )
    )

    (T
      (setq val cutSize)
    )
  )

  ; The cross block uses positive handle distances. A non-positive value means
  ; the requested cut is too large for this block/origin configuration. Clamp
  ; very slightly positive so AutoCAD does not reject the whole insert; the
  ; preview SMALL/max checks should normally prevent such cases.
  (if (< val minVal) (setq val minVal))

  val
)

(defun fcd:v25-cut-size-for-key (cuts key / v)
  (setq v (cdr (assoc key cuts)))
  (if (numberp v) (abs v) 0.0)
)

(defun fcd:v25-set-all-corner-props (obj cuts / ok keys key cutSize panelW panelH handleValue)
  (setq ok T)
  (setq panelW (fcd:v25-get-dynprop-number obj "L"))
  (setq panelH (fcd:v25-get-dynprop-number obj "H"))

  (if (or (not panelW) (not panelH) (<= panelW 0.0) (<= panelH 0.0))
    (setq ok nil)
    (progn
      (setq keys (fcd:v23-cut-param-keys))

      ; Critical V25 change: write ALL corner parameters every time.
      ; RECT = all cut sizes 0; L/T = active corners non-zero and inactive 0.
      (foreach key keys
        (setq cutSize (fcd:v25-cut-size-for-key cuts key))
        (setq handleValue (fcd:v25-target-corner-handle-value obj key cutSize panelW panelH))

        (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name key) (* 1.0 handleValue)))
          (setq ok nil)
        )
      )
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

; Override V23/V24 writer. The calling pipeline remains:
; insert -> reset -> L/H -> align full/default bbox -> write cuts -> no further move.
(defun fcd:v23-set-active-cut-props (obj cuts)
  (fcd:v25-set-all-corner-props obj cuts)
)

(defun fcd:v25-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V25: RECT/L/T вставляются блоком "
          blockName
          "; L/H -> выравнивание -> запись ВСЕХ углов по формуле half-size-minus-cut."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V25. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )

              (if (> (cdr (assoc 'failed stats)) 0)
                (fcd:msg "Если есть ошибки вставки/динамики: вероятно, вырез слишком большой для origin ручек блока; проверь max/min и FCD_DUMP_DYNPROPS.")
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v25-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v25-panel-build-command)
)

(princ "\nFCD_CORE_V25 loaded: RECT/L/T cross-block cuts are written as neutral/full-frame handle values.")
(princ)


; ============================================================
; V26 LOCAL-BBOX CALIBRATED CROSS-BLOCK MODEL
;
; The uploaded drawing showed that the V25 model was still wrong:
; it assumed that FCD_Panel is centered around 0 and used L/2 / H/2 as
; the outer frame. In the real block definition the dynamic frame is not
; necessarily centered: after L/H the outer frame is whatever the block bbox is.
;
; Correct model for this FCD_Panel:
;   1) Insert block.
;   2) Reset, set L/H.
;   3) Read the current local bbox of the block. This bbox is the real full
;      cassette frame after L/H, even if the default block is cross-shaped.
;   4) Write ALL corner handles using the real local bbox:
;
;      LEFT  X: value = Origin.X - (bbox.minX + cutX)
;      RIGHT X: value = (bbox.maxX - cutX) - Origin.X
;
;      TOP    Y: value = (bbox.maxY - cutY) - Origin.Y
;      BOTTOM Y: value = Origin.Y - (bbox.minY + cutY)
;
;      RECT means cutX/cutY = 0 for all corners.
;
;   5) Update, then align final visible bbox to the target frame.
;
; This fixes the V25 "inside out" behaviour caused by using L/2/H/2 instead
; of the actual dynamic block local frame.
; ============================================================

(defun fcd:v26-local-bbox (obj / bbox ins ix iy minx miny maxx maxy)
  (setq bbox (fcd:v14-bbox obj))
  (setq ins (fcd:v22-block-insertion-point obj))

  (if (and bbox ins)
    (progn
      (setq ix (car ins))
      (setq iy (cadr ins))
      (setq minx (- (fcd:v14-bbox-min-x bbox) ix))
      (setq miny (- (fcd:v14-bbox-min-y bbox) iy))
      (setq maxx (- (fcd:v14-bbox-max-x bbox) ix))
      (setq maxy (- (fcd:v14-bbox-max-y bbox) iy))
      (list minx miny maxx maxy)
    )
    nil
  )
)

(defun fcd:v26-origin-local (obj key / info origin)
  (setq info (fcd:v22-find-dynprop-with-origin obj (vl-symbol-name key)))
  (setq origin (cdr (assoc 'origin info)))
  (if (and origin (listp origin) (>= (length origin) 2))
    origin
    nil
  )
)

(defun fcd:v26-cut-key-axis (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (if (wcmatch s "*_X") 'X 'Y)
)

(defun fcd:v26-cut-key-x-side (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (cond
    ((or (= s "LT_X") (= s "LB_X")) 'LEFT)
    ((or (= s "RT_X") (= s "RB_X")) 'RIGHT)
    (T nil)
  )
)

(defun fcd:v26-cut-key-y-side (key / s)
  (setq s (strcase (vl-symbol-name key)))
  (cond
    ((or (= s "LT_Y") (= s "RT_Y")) 'TOP)
    ((or (= s "LB_Y") (= s "RB_Y")) 'BOTTOM)
    (T nil)
  )
)

(defun fcd:v26-positive-handle (v / minVal)
  (setq minVal 0.001)
  (cond
    ((not (numberp v)) nil)
    ((< v minVal) minVal)
    (T v)
  )
)

(defun fcd:v26-target-corner-handle-value (obj key cutSize localFrame / origin axis side ox oy minx miny maxx maxy inner val)
  (setq origin (fcd:v26-origin-local obj key))
  (setq axis (fcd:v26-cut-key-axis key))

  (if (or (not origin) (not localFrame))
    nil
    (progn
      (setq ox (car origin))
      (setq oy (cadr origin))
      (setq minx (nth 0 localFrame))
      (setq miny (nth 1 localFrame))
      (setq maxx (nth 2 localFrame))
      (setq maxy (nth 3 localFrame))

      (cond
        ((= axis 'X)
          (setq side (fcd:v26-cut-key-x-side key))
          (cond
            ((= side 'LEFT)
              ; Inner vertical edge of a left corner cut.
              ; No cut: inner = minx.
              (setq inner (+ minx cutSize))
              (setq val (- ox inner))
            )
            ((= side 'RIGHT)
              ; Inner vertical edge of a right corner cut.
              ; No cut: inner = maxx.
              (setq inner (- maxx cutSize))
              (setq val (- inner ox))
            )
            (T
              (setq val nil)
            )
          )
        )

        ((= axis 'Y)
          (setq side (fcd:v26-cut-key-y-side key))
          (cond
            ((= side 'TOP)
              ; Inner horizontal edge of a top corner cut.
              ; No cut: inner = maxy.
              (setq inner (- maxy cutSize))
              (setq val (- inner oy))
            )
            ((= side 'BOTTOM)
              ; Inner horizontal edge of a bottom corner cut.
              ; No cut: inner = miny.
              (setq inner (+ miny cutSize))
              (setq val (- oy inner))
            )
            (T
              (setq val nil)
            )
          )
        )

        (T
          (setq val nil)
        )
      )

      (fcd:v26-positive-handle val)
    )
  )
)

(defun fcd:v26-cut-size-for-key (cuts key / v)
  (setq v (cdr (assoc key cuts)))
  (if (numberp v) (abs v) 0.0)
)

(defun fcd:v26-set-all-corner-props (obj cuts localFrame / ok keys key cutSize handleValue)
  (setq ok T)
  (setq keys (fcd:v23-cut-param-keys))

  ; Critical: write all eight corner handles for every panel.
  ; RECT = all cut sizes 0. L/T = only detected missing-corner sizes non-zero.
  (foreach key keys
    (setq cutSize (fcd:v26-cut-size-for-key cuts key))
    (setq handleValue (fcd:v26-target-corner-handle-value obj key (* 1.0 cutSize) localFrame))

    (if (not handleValue)
      (setq ok nil)
      (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name key) (* 1.0 handleValue)))
        (setq ok nil)
      )
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v26-insert-panel-shape (bbox cuts / x1 y1 x2 y2 w h blockName obj tempPt lhOk localFrame cutOk alignOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      ; Insert close to target; final position is done by bbox alignment after
      ; all dynamic geometry is already written.
      (setq tempPt (list x1 y1 0.0))
      (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)
          (fcd:v22-reset-block-if-possible obj)

          ; Set L/H first and read the real local full-frame bbox after that.
          (setq lhOk (fcd:v21-set-panel-lh-only obj w h))
          (vl-catch-all-apply 'vla-Update (list obj))

          (setq localFrame nil)
          (if lhOk
            (setq localFrame (fcd:v26-local-bbox obj))
          )

          ; Write all eight corners using the calibrated local frame.
          (setq cutOk nil)
          (if localFrame
            (setq cutOk (fcd:v26-set-all-corner-props obj cuts localFrame))
          )

          ; Now that the final shape exists, align final visible bbox to target.
          ; Moving the reference after dynamic edits is safe; it does not change
          ; local dynamic handle values.
          (setq alignOk nil)
          (if cutOk
            (setq alignOk (fcd:v14-align-bbox-min-to-rect obj x1 y1))
          )

          (cond
            ((and lhOk localFrame cutOk alignOk)
              (vl-catch-all-apply 'vla-Update (list obj))
              obj
            )
            (T
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

; Override insertion hook used by fcd:v17-build-panel-blocks.
(defun fcd:v17-insert-panel-shape (bbox cuts)
  (fcd:v26-insert-panel-shape bbox cuts)
)

(defun fcd:v26-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V26: RECT/L/T вставляются блоком "
          blockName
          "; углы пишутся по реальному локальному bbox после L/H, а не через L/2/H/2."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V26. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v26-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v26-panel-build-command)
)

(princ "\nFCD_CORE_V26 loaded: cross-block corner handles use local bbox calibrated values.")
(princ)


; ============================================================
; V27 DYNAMIC-ORIGIN FULL-FRAME ANCHOR MODEL
;
; V26 was close, but it still used the visible block bbox as the reference
; frame. For a cross-shaped FCD_Panel the visible bbox is not the real full
; L x H frame: it already depends on the current corner handle positions.
;
; V27 uses the dynamic parameter ORIGIN points to reconstruct the true full
; frame after L/H:
;   minX = Origin.X of LT_Y/LB_Y
;   maxX = Origin.X of RT_Y/RB_Y
;   minY = Origin.Y of LB_X/RB_X
;   maxY = Origin.Y of LT_X/RT_X
;
; Build order:
;   1) insert/reset
;   2) set L/H
;   3) read true full frame from dynamic origins
;   4) align that full frame to the target grid bbox
;   5) reread origins and write all eight corner handles
;   6) do NOT move after cuts
;
; This keeps the cassette full frame fixed in the grid, while all cuts move
; inward from that frame.
; ============================================================

(defun fcd:v27-avg2 (a b)
  (cond
    ((and (numberp a) (numberp b)) (/ (+ a b) 2.0))
    ((numberp a) a)
    ((numberp b) b)
    (T nil)
  )
)

(defun fcd:v27-dist2 (p q / dx dy)
  (setq dx (- (car p) (car q)))
  (setq dy (- (cadr p) (cadr q)))
  (+ (* dx dx) (* dy dy))
)

(defun fcd:v27-origin-raw (obj key / info origin)
  (setq info (fcd:v22-find-dynprop-with-origin obj (vl-symbol-name key)))
  (setq origin (cdr (assoc 'origin info)))
  (if (and origin (listp origin) (>= (length origin) 2))
    origin
    nil
  )
)

(defun fcd:v27-frame-from-origins (obj / oLTY oLBY oRTY oRBY oLTX oRTX oLBX oRBX minx maxx miny maxy)
  ; X frame comes from Y-handle origins at left/right sides.
  (setq oLTY (fcd:v27-origin-raw obj 'LT_Y))
  (setq oLBY (fcd:v27-origin-raw obj 'LB_Y))
  (setq oRTY (fcd:v27-origin-raw obj 'RT_Y))
  (setq oRBY (fcd:v27-origin-raw obj 'RB_Y))

  ; Y frame comes from X-handle origins at top/bottom sides.
  (setq oLTX (fcd:v27-origin-raw obj 'LT_X))
  (setq oRTX (fcd:v27-origin-raw obj 'RT_X))
  (setq oLBX (fcd:v27-origin-raw obj 'LB_X))
  (setq oRBX (fcd:v27-origin-raw obj 'RB_X))

  (setq minx (fcd:v27-avg2 (if oLTY (car oLTY) nil) (if oLBY (car oLBY) nil)))
  (setq maxx (fcd:v27-avg2 (if oRTY (car oRTY) nil) (if oRBY (car oRBY) nil)))
  (setq maxy (fcd:v27-avg2 (if oLTX (cadr oLTX) nil) (if oRTX (cadr oRTX) nil)))
  (setq miny (fcd:v27-avg2 (if oLBX (cadr oLBX) nil) (if oRBX (cadr oRBX) nil)))

  (if (and (numberp minx) (numberp miny) (numberp maxx) (numberp maxy)
           (> (- maxx minx) (fcd:get 'tol))
           (> (- maxy miny) (fcd:get 'tol)))
    (list minx miny maxx maxy)
    nil
  )
)

(defun fcd:v27-bbox-center (bbox)
  (list
    (/ (+ (fcd:v14-bbox-min-x bbox) (fcd:v14-bbox-max-x bbox)) 2.0)
    (/ (+ (fcd:v14-bbox-min-y bbox) (fcd:v14-bbox-max-y bbox)) 2.0)
  )
)

(defun fcd:v27-frame-center (frame)
  (list
    (/ (+ (nth 0 frame) (nth 2 frame)) 2.0)
    (/ (+ (nth 1 frame) (nth 3 frame)) 2.0)
  )
)

(defun fcd:v27-origin-mode (obj frame / bbox ins rawC wcsC localC dW dL)
  ; Dynamic-property Origin may be exposed either in block-local coordinates
  ; or in WCS depending on AutoCAD/context. Detect it by comparing centers.
  (setq bbox (fcd:v14-bbox obj))
  (setq ins (fcd:v22-block-insertion-point obj))

  (if (and bbox ins frame)
    (progn
      (setq rawC (fcd:v27-frame-center frame))
      (setq wcsC (fcd:v27-bbox-center bbox))
      (setq localC (list (- (car wcsC) (car ins)) (- (cadr wcsC) (cadr ins))))
      (setq dW (fcd:v27-dist2 rawC wcsC))
      (setq dL (fcd:v27-dist2 rawC localC))
      (if (< dW dL) 'WCS 'LOCAL)
    )
    'LOCAL
  )
)

(defun fcd:v27-frame-wcs (obj frame mode / ins)
  (if (= mode 'WCS)
    frame
    (progn
      (setq ins (fcd:v22-block-insertion-point obj))
      (list
        (+ (nth 0 frame) (car ins))
        (+ (nth 1 frame) (cadr ins))
        (+ (nth 2 frame) (car ins))
        (+ (nth 3 frame) (cadr ins))
      )
    )
  )
)

(defun fcd:v27-align-frame-min-to-rect (obj frame mode x1 y1 / fw dx dy)
  (setq fw (fcd:v27-frame-wcs obj frame mode))
  (if fw
    (progn
      (setq dx (- x1 (nth 0 fw)))
      (setq dy (- y1 (nth 1 fw)))
      (fcd:v14-move-object obj dx dy)
      (vl-catch-all-apply 'vla-Update (list obj))
      T
    )
    nil
  )
)

(defun fcd:v27-positive-handle (v / tol minVal)
  (setq tol 0.05)
  (setq minVal 0.001)
  (cond
    ((not (numberp v)) nil)
    ((>= v minVal) v)
    ((> v (- tol)) minVal)
    (T nil)
  )
)

(defun fcd:v27-target-corner-handle-value (obj key cutSize frame / origin axis side ox oy minx miny maxx maxy inner val)
  (setq origin (fcd:v27-origin-raw obj key))
  (setq axis (fcd:v26-cut-key-axis key))

  (if (or (not origin) (not frame))
    nil
    (progn
      (setq ox (car origin))
      (setq oy (cadr origin))
      (setq minx (nth 0 frame))
      (setq miny (nth 1 frame))
      (setq maxx (nth 2 frame))
      (setq maxy (nth 3 frame))

      (cond
        ((= axis 'X)
          (setq side (fcd:v26-cut-key-x-side key))
          (cond
            ((= side 'LEFT)
              (setq inner (+ minx cutSize))
              (setq val (- ox inner))
            )
            ((= side 'RIGHT)
              (setq inner (- maxx cutSize))
              (setq val (- inner ox))
            )
            (T (setq val nil))
          )
        )

        ((= axis 'Y)
          (setq side (fcd:v26-cut-key-y-side key))
          (cond
            ((= side 'TOP)
              (setq inner (- maxy cutSize))
              (setq val (- inner oy))
            )
            ((= side 'BOTTOM)
              (setq inner (+ miny cutSize))
              (setq val (- oy inner))
            )
            (T (setq val nil))
          )
        )

        (T (setq val nil))
      )

      (fcd:v27-positive-handle val)
    )
  )
)

(defun fcd:v27-set-all-corner-props (obj cuts frame / ok keys key cutSize handleValue)
  (setq ok T)
  (setq keys (fcd:v23-cut-param-keys))

  ; Write all corners. RECT = every cut size is 0.
  (foreach key keys
    (setq cutSize (fcd:v26-cut-size-for-key cuts key))
    (setq handleValue (fcd:v27-target-corner-handle-value obj key (* 1.0 cutSize) frame))

    (if (not handleValue)
      (setq ok nil)
      (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name key) (* 1.0 handleValue)))
        (setq ok nil)
      )
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v27-insert-panel-shape (bbox cuts / x1 y1 x2 y2 w h blockName obj tempPt lhOk frame mode alignOk cutOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      ; Insert near the target frame. Exact anchoring is done through the
      ; dynamic-origin full frame after L/H.
      (setq tempPt (list x1 y1 0.0))
      (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)
          (fcd:v22-reset-block-if-possible obj)

          ; 1) Set the full panel dimensions.
          (setq lhOk (fcd:v21-set-panel-lh-only obj w h))
          (vl-catch-all-apply 'vla-Update (list obj))

          ; 2) Use dynamic origins, not visible bbox, to get full L x H frame.
          (setq frame nil)
          (setq mode 'LOCAL)
          (if lhOk
            (progn
              (setq frame (fcd:v27-frame-from-origins obj))
              (if frame (setq mode (fcd:v27-origin-mode obj frame)))
            )
          )

          ; 3) Anchor the true full frame to the grid rectangle BEFORE cuts.
          (setq alignOk nil)
          (if frame
            (setq alignOk (fcd:v27-align-frame-min-to-rect obj frame mode x1 y1))
          )

          ; 4) Reread origins after moving, because some AutoCAD builds expose
          ;    them in WCS. Then write all corner handles.
          (setq frame nil)
          (if alignOk
            (setq frame (fcd:v27-frame-from-origins obj))
          )

          (setq cutOk nil)
          (if frame
            (setq cutOk (fcd:v27-set-all-corner-props obj cuts frame))
          )

          ; Do not align after cuts. The full L x H frame is already fixed.
          (cond
            ((and lhOk alignOk cutOk)
              (vl-catch-all-apply 'vla-Update (list obj))
              obj
            )
            (T
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v17-insert-panel-shape (bbox cuts)
  (fcd:v27-insert-panel-shape bbox cuts)
)

(defun fcd:v27-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Блок FCD_Panel не найден в чертеже. Вставь/создай блок с точным именем FCD_Panel.")
    (progn
      (fcd:msg
        (strcat
          "Выбери всю геометрию раскладки. V27: полный L/H-габарит берется из Origin динамических ручек, "
          "сначала фиксируется в сетке, потом пишутся угловые вырезы."
        )
      )

      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "Геометрия не выбрана.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))

          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Недостаточно геометрии для построения областей: нужно минимум 2 X и 2 Y координаты.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))

              (fcd:msg
                (strcat
                  "Вставка панелей завершена V27. "
                  "Всего вставлено: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P резерв: " (itoa (cdr (assoc 'skippedU stats)))
                  ", неподдерживаемых: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", ошибок расчета вырезов: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", ошибок вставки/динамики: " (itoa (cdr (assoc 'failed stats)))
                  ", невалидных: " (itoa (cdr (assoc 'invalid stats)))
                  ", служебных/preview пропущено: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", объектов/сегментов проигнорировано: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )
            )
          )
        )
      )
    )
  )

  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v27-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v27-panel-build-command)
)

(princ "\nFCD_CORE_V27 loaded: full frame is anchored from dynamic Origin points before cuts.")
(princ)

; ============================================================
; V29 - CLEAN V27 + INTERNAL ZERO FALLBACK
;
; V27 is kept as the primary path because its evaluated anonymous blocks match
; the target T contour in the stable drawings. V29 adds a fallback only for
; cases where V27 cannot produce positive corner handle values, typically small
; rectangles or L/T positions where the block zero would fall beyond a cut.
; ============================================================

(defun fcd:service-preview-layer-p (layerName / u)
  (setq u (strcase layerName))
  (or
    (wcmatch u "FCD_PREVIEW_*")
    (= u "FCD_PANELS")
    (= u "FCD_TRIMS")
    (= u "FCD_PANEL_GUIDES")
  )
)

(defun fcd:init-layers ()
  (fcd:ensure-layer "FCD_BOUNDARY" 7)
  (fcd:ensure-layer "FCD_OPENINGS" 1)
  (fcd:ensure-layer "FCD_GRID_AXIS" 8)
  (fcd:ensure-layer "FCD_GRID_EDGE" 8)
  (fcd:ensure-layer "FCD_PREVIEW_OK" 3)
  (fcd:ensure-layer "FCD_PREVIEW_WARN" 2)
  (fcd:ensure-layer "FCD_PREVIEW_ERR" 1)
  (fcd:ensure-layer "FCD_PREVIEW_LCUT" 4)
  (fcd:ensure-layer "FCD_PREVIEW_TCUT" 6)
  (fcd:ensure-layer "FCD_PREVIEW_VOID" 8)
  (fcd:ensure-layer "FCD_PREVIEW_UCUT" 200)
  (fcd:ensure-layer "FCD_PANELS" 4)
  (fcd:ensure-layer "FCD_TRIMS" 5)
  (fcd:ensure-layer "FCD_PANEL_GUIDES" 8)
  (princ)
)

(defun fcd:v29-force-unit-scale (obj)
  (vl-catch-all-apply 'vla-put-XScaleFactor (list obj 1.0))
  (vl-catch-all-apply 'vla-put-YScaleFactor (list obj 1.0))
  (vl-catch-all-apply 'vla-put-ZScaleFactor (list obj 1.0))
  T
)

(defun fcd:v29-target-edge (bbox cuts key / axis side cut)
  (setq axis (fcd:v26-cut-key-axis key))
  (setq cut (fcd:v26-cut-size-for-key cuts key))
  (cond
    ((= axis 'X)
      (setq side (fcd:v26-cut-key-x-side key))
      (cond
        ((= side 'LEFT) (+ (nth 0 bbox) cut))
        ((= side 'RIGHT) (- (nth 2 bbox) cut))
        (T nil)
      )
    )
    ((= axis 'Y)
      (setq side (fcd:v26-cut-key-y-side key))
      (cond
        ((= side 'TOP) (- (nth 3 bbox) cut))
        ((= side 'BOTTOM) (+ (nth 1 bbox) cut))
        (T nil)
      )
    )
    (T nil)
  )
)

(defun fcd:v29-anchor-point (bbox cuts / keys key axis side target xLo xHi yLo yHi)
  ; The effective zero must be right of every left edge, left of every right
  ; edge, above every bottom edge, and below every top edge.
  (setq xLo (nth 0 bbox))
  (setq xHi (nth 2 bbox))
  (setq yLo (nth 1 bbox))
  (setq yHi (nth 3 bbox))
  (setq keys (fcd:v23-cut-param-keys))

  (foreach key keys
    (setq target (fcd:v29-target-edge bbox cuts key))
    (setq axis (fcd:v26-cut-key-axis key))
    (if (numberp target)
      (cond
        ((= axis 'X)
          (setq side (fcd:v26-cut-key-x-side key))
          (cond
            ((= side 'LEFT) (setq xLo (max xLo target)))
            ((= side 'RIGHT) (setq xHi (min xHi target)))
          )
        )
        ((= axis 'Y)
          (setq side (fcd:v26-cut-key-y-side key))
          (cond
            ((= side 'BOTTOM) (setq yLo (max yLo target)))
            ((= side 'TOP) (setq yHi (min yHi target)))
          )
        )
      )
    )
  )

  (if (and (< xLo xHi) (< yLo yHi))
    (list (/ (+ xLo xHi) 2.0) (/ (+ yLo yHi) 2.0))
    nil
  )
)

(defun fcd:v29-origin-wcs (obj key mode / origin ins)
  (setq origin (fcd:v27-origin-raw obj key))
  (if origin
    (if (= mode 'WCS)
      origin
      (progn
        (setq ins (fcd:v22-block-insertion-point obj))
        (if ins
          (list (+ (car ins) (car origin)) (+ (cadr ins) (cadr origin)))
          nil
        )
      )
    )
    nil
  )
)

(defun fcd:v29-avg-list (vals / sum n v)
  (setq sum 0.0)
  (setq n 0)
  (foreach v vals
    (if (numberp v)
      (progn
        (setq sum (+ sum v))
        (setq n (1+ n))
      )
    )
  )
  (if (> n 0) (/ sum n) nil)
)

(defun fcd:v29-effective-zero-wcs (obj mode / xVals yVals key p)
  (setq xVals nil)
  (setq yVals nil)

  (foreach key (list 'LT_X 'RT_X 'RB_X 'LB_X)
    (setq p (fcd:v29-origin-wcs obj key mode))
    (if p (setq xVals (cons (car p) xVals)))
  )

  (foreach key (list 'LT_Y 'RT_Y 'RB_Y 'LB_Y)
    (setq p (fcd:v29-origin-wcs obj key mode))
    (if p (setq yVals (cons (cadr p) yVals)))
  )

  (if (and xVals yVals)
    (list (fcd:v29-avg-list xVals) (fcd:v29-avg-list yVals))
    nil
  )
)

(defun fcd:v29-direct-handle-value (obj bbox cuts key mode / origin target axis side val)
  (setq origin (fcd:v29-origin-wcs obj key mode))
  (setq target (fcd:v29-target-edge bbox cuts key))
  (setq axis (fcd:v26-cut-key-axis key))
  (setq val nil)

  (if (and origin (numberp target))
    (cond
      ((= axis 'X)
        (setq side (fcd:v26-cut-key-x-side key))
        (cond
          ((= side 'LEFT) (setq val (- (car origin) target)))
          ((= side 'RIGHT) (setq val (- target (car origin))))
        )
      )
      ((= axis 'Y)
        (setq side (fcd:v26-cut-key-y-side key))
        (cond
          ((= side 'TOP) (setq val (- target (cadr origin))))
          ((= side 'BOTTOM) (setq val (- (cadr origin) target)))
        )
      )
    )
  )

  (fcd:v27-positive-handle val)
)

(defun fcd:v29-set-direct-corner-props (obj bbox cuts mode / ok keys key handleValue)
  (setq ok T)
  (setq keys (fcd:v23-cut-param-keys))

  (foreach key keys
    (setq handleValue (fcd:v29-direct-handle-value obj bbox cuts key mode))
    (if (not handleValue)
      (setq ok nil)
      (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name key) (* 1.0 handleValue)))
        (setq ok nil)
      )
    )
  )

  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v29-insert-panel-shape-anchor (bbox cuts / x1 y1 x2 y2 w h blockName obj tempPt lhOk frame mode alignOk anchor zero dx dy cutOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      (setq anchor (fcd:v29-anchor-point bbox cuts))
      (if anchor
        (progn
          (setq tempPt (list x1 y1 0.0))
          (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

          (if obj
            (progn
              (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
              (fcd:v16-reset-block-scale obj)
              (fcd:v29-force-unit-scale obj)
              (fcd:v22-reset-block-if-possible obj)
              (fcd:v29-force-unit-scale obj)

              (setq lhOk (fcd:v21-set-panel-lh-only obj w h))
              (vl-catch-all-apply 'vla-Update (list obj))

              (setq frame nil)
              (setq mode 'LOCAL)
              (if lhOk
                (progn
                  (setq frame (fcd:v27-frame-from-origins obj))
                  (if frame (setq mode (fcd:v27-origin-mode obj frame)))
                )
              )

              (setq alignOk nil)
              (if frame
                (setq alignOk (fcd:v27-align-frame-min-to-rect obj frame mode x1 y1))
              )

              (setq frame nil)
              (if alignOk
                (progn
                  (setq frame (fcd:v27-frame-from-origins obj))
                  (if frame (setq mode (fcd:v27-origin-mode obj frame)))
                )
              )

              (setq zero nil)
              (if frame
                (setq zero (fcd:v29-effective-zero-wcs obj mode))
              )

              (if zero
                (progn
                  (setq dx (- (car anchor) (car zero)))
                  (setq dy (- (cadr anchor) (cadr zero)))
                  (if (or (> (abs dx) 0.000001) (> (abs dy) 0.000001))
                    (progn
                      (fcd:v14-move-object obj dx dy)
                      (vl-catch-all-apply 'vla-Update (list obj))
                    )
                  )
                  (setq frame (fcd:v27-frame-from-origins obj))
                  (if frame (setq mode (fcd:v27-origin-mode obj frame)))
                )
              )

              (setq cutOk nil)
              (if (and frame zero)
                (setq cutOk (fcd:v29-set-direct-corner-props obj bbox cuts mode))
              )

              (if (and lhOk alignOk zero cutOk)
                (progn
                  (vl-catch-all-apply 'vla-Update (list obj))
                  obj
                )
                (progn
                  (fcd:v14-delete-object obj)
                  nil
                )
              )
            )
          )
        )
      )
    )
  )
)

(defun fcd:v29-insert-panel-shape (bbox cuts / obj)
  ; Stable V27 first. If its positive-handle model cannot fit the target,
  ; fall back to placing the effective zero inside the target polygon.
  (setq obj (fcd:v27-insert-panel-shape bbox cuts))
  (if obj
    obj
    (fcd:v29-insert-panel-shape-anchor bbox cuts)
  )
)

(defun fcd:v17-insert-panel-shape (bbox cuts)
  (fcd:v29-insert-panel-shape bbox cuts)
)

(defun fcd:v29-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Block FCD_Panel not found in drawing.")
    (progn
      ; Generated blocks are cleared before every build. This keeps repeated
      ; tests from feeding old failed geometry back into visual inspection.
      (fcd:delete-layer-objects "FCD_PANELS")
      (fcd:delete-layer-objects "FCD_PANEL_GUIDES")

      (fcd:msg "Select layout geometry. V29: stable V27 insert, internal-zero fallback for hard panels.")
      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "No geometry selected.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))
          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Not enough geometry: at least 2 X and 2 Y coordinates are required.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))
              (fcd:msg
                (strcat
                  "Panel build complete V29. Inserted: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P reserved: " (itoa (cdr (assoc 'skippedU stats)))
                  ", unsupported: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", cut calculation errors: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", insert/dynamic errors: " (itoa (cdr (assoc 'failed stats)))
                  ", invalid: " (itoa (cdr (assoc 'invalid stats)))
                  ", service/preview skipped: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", ignored objects/segments: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v29-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v29-panel-build-command)
)

(princ "\nFCD_CORE_V29 loaded: clean V27 with internal-zero fallback.")
(princ)

; ============================================================
; V30 - VIRTUAL FRAME FALLBACK
;
; V29 moved the effective zero into the safe area but kept the evaluated L/H
; frame at the original target size. That can leave the target contour outside
; the dynamic block's working frame, so grips appear to pull in wrong directions.
; V30 enlarges the fallback L/H frame around the safe zero first, then writes
; all grips back to the real target contour.
; ============================================================

(defun fcd:v30-safe-margin (/ tol)
  (setq tol (fcd:get 'tol))
  (if (not (numberp tol)) (setq tol 0.01))
  (max 0.001 (* 2.0 tol))
)

(defun fcd:v30-clamp (v lo hi)
  (max lo (min hi v))
)

(defun fcd:v30-safe-box (bbox cuts / keys key axis side target margin xLo xHi yLo yHi)
  (setq margin (fcd:v30-safe-margin))
  (setq xLo (nth 0 bbox))
  (setq xHi (nth 2 bbox))
  (setq yLo (nth 1 bbox))
  (setq yHi (nth 3 bbox))

  (foreach key (fcd:v23-cut-param-keys)
    (setq target (fcd:v29-target-edge bbox cuts key))
    (setq axis (fcd:v26-cut-key-axis key))
    (if (numberp target)
      (cond
        ((= axis 'X)
          (setq side (fcd:v26-cut-key-x-side key))
          (cond
            ((= side 'LEFT) (setq xLo (max xLo (+ target margin))))
            ((= side 'RIGHT) (setq xHi (min xHi (- target margin))))
          )
        )
        ((= axis 'Y)
          (setq side (fcd:v26-cut-key-y-side key))
          (cond
            ((= side 'BOTTOM) (setq yLo (max yLo (+ target margin))))
            ((= side 'TOP) (setq yHi (min yHi (- target margin))))
          )
        )
      )
    )
  )

  (if (and (< xLo xHi) (< yLo yHi))
    (list xLo yLo xHi yHi)
    nil
  )
)

(defun fcd:v30-fallback-plan (bbox cuts zero frameWcs / safe x1 y1 x2 y2 w h zx zy leftOff rightOff bottomOff topOff ax ay lReq hReq maxAx minAy)
  (if (and zero frameWcs)
    (progn
      (setq safe (fcd:v30-safe-box bbox cuts))
      (if safe
        (progn
          (setq x1 (nth 0 bbox))
          (setq y1 (nth 1 bbox))
          (setq x2 (nth 2 bbox))
          (setq y2 (nth 3 bbox))
          (setq w (- x2 x1))
          (setq h (- y2 y1))
          (setq zx (car zero))
          (setq zy (cadr zero))
          (setq leftOff (- zx (nth 0 frameWcs)))
          (setq rightOff (- (nth 2 frameWcs) zx))
          (setq bottomOff (- zy (nth 1 frameWcs)))
          (setq topOff (- (nth 3 frameWcs) zy))

          ; Keep V29 insertion behavior: place the effective zero in the
          ; middle of the safe internal area, then only enlarge the virtual
          ; frame as much as needed for predictable grip directions.
          (setq ax (/ (+ (nth 0 safe) (nth 2 safe)) 2.0))
          (setq ay (/ (+ (nth 1 safe) (nth 3 safe)) 2.0))

          ; Left and top offsets are the fixed side of this dynamic block.
          ; If the anchor moves too far, the virtual frame would no longer cover
          ; the real target contour. Pull it back inside the safe interval.
          (setq maxAx (+ x1 leftOff))
          (if (> ax maxAx) (setq ax maxAx))
          (setq minAy (- y2 topOff))
          (if (< ay minAy) (setq ay minAy))

          (if (and
                (>= ax (nth 0 safe))
                (<= ax (nth 2 safe))
                (>= ay (nth 1 safe))
                (<= ay (nth 3 safe))
                (> leftOff 0.0)
                (> topOff 0.0)
              )
            (progn
              ; The right and bottom sides can be made larger by increasing L/H.
              (setq lReq (max w (+ leftOff (- x2 ax))))
              (setq hReq (max h (+ topOff (- ay y1))))
              (if (and (> lReq (fcd:get 'tol)) (> hReq (fcd:get 'tol)))
                (list
                  (cons 'anchor (list ax ay))
                  (cons 'L lReq)
                  (cons 'H hReq)
                )
                nil
              )
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

(defun fcd:v30-adjusted-cut-size (bbox cuts frameWcs key / target axis side val tol)
  ; Convert the real target contour into cut distances from the enlarged
  ; virtual frame. Then V27's proven Origin/frame formula can write the grips.
  (setq target (fcd:v29-target-edge bbox cuts key))
  (setq axis (fcd:v26-cut-key-axis key))
  (setq tol (fcd:get 'tol))
  (setq val nil)

  (if (and frameWcs (numberp target))
    (cond
      ((= axis 'X)
        (setq side (fcd:v26-cut-key-x-side key))
        (cond
          ((= side 'LEFT) (setq val (- target (nth 0 frameWcs))))
          ((= side 'RIGHT) (setq val (- (nth 2 frameWcs) target)))
        )
      )
      ((= axis 'Y)
        (setq side (fcd:v26-cut-key-y-side key))
        (cond
          ((= side 'TOP) (setq val (- (nth 3 frameWcs) target)))
          ((= side 'BOTTOM) (setq val (- target (nth 1 frameWcs))))
        )
      )
    )
  )

  (cond
    ((not (numberp val)) nil)
    ((< val (- tol)) nil)
    ((< val 0.0) 0.0)
    (T val)
  )
)

(defun fcd:v30-adjusted-cuts-for-frame (bbox cuts frameWcs / ok result keys key v active)
  (setq ok T)
  (setq result (list (cons 'valid T)))
  (setq active 0)
  (setq keys (fcd:v23-cut-param-keys))

  (foreach key keys
    (setq v (fcd:v30-adjusted-cut-size bbox cuts frameWcs key))
    (if (not (numberp v))
      (setq ok nil)
      (progn
        (if (> v (fcd:get 'tol)) (setq active (1+ active)))
        (setq result (cons (cons key v) result))
      )
    )
  )

  (if ok
    (cons (cons 'active active) result)
    nil
  )
)

(defun fcd:v30-set-frame-fit-props (obj bbox cuts frame mode / frameWcs adjusted)
  (setq frameWcs (fcd:v27-frame-wcs obj frame mode))
  (setq adjusted (fcd:v30-adjusted-cuts-for-frame bbox cuts frameWcs))
  (if adjusted
    (fcd:v27-set-all-corner-props obj adjusted frame)
    nil
  )
)

(defun fcd:v30-insert-panel-shape-anchor (bbox cuts / x1 y1 x2 y2 w h blockName obj tempPt lhOk frame mode frameWcs alignOk zero plan anchor lReq hReq dx dy cutOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      (setq tempPt (list x1 y1 0.0))
      (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)
          (fcd:v29-force-unit-scale obj)
          (fcd:v22-reset-block-if-possible obj)
          (fcd:v29-force-unit-scale obj)

          ; First evaluate the natural target-sized frame.
          (setq lhOk (fcd:v21-set-panel-lh-only obj w h))
          (vl-catch-all-apply 'vla-Update (list obj))

          (setq frame nil)
          (setq mode 'LOCAL)
          (if lhOk
            (progn
              (setq frame (fcd:v27-frame-from-origins obj))
              (if frame (setq mode (fcd:v27-origin-mode obj frame)))
            )
          )

          (setq alignOk nil)
          (if frame
            (setq alignOk (fcd:v27-align-frame-min-to-rect obj frame mode x1 y1))
          )

          (setq frame nil)
          (setq frameWcs nil)
          (setq zero nil)
          (if alignOk
            (progn
              (setq frame (fcd:v27-frame-from-origins obj))
              (if frame
                (progn
                  (setq mode (fcd:v27-origin-mode obj frame))
                  (setq frameWcs (fcd:v27-frame-wcs obj frame mode))
                  (setq zero (fcd:v29-effective-zero-wcs obj mode))
                )
              )
            )
          )

          (setq plan (fcd:v30-fallback-plan bbox cuts zero frameWcs))

          (if plan
            (progn
              (setq anchor (cdr (assoc 'anchor plan)))
              (setq lReq (cdr (assoc 'L plan)))
              (setq hReq (cdr (assoc 'H plan)))

              ; Re-evaluate with the enlarged virtual frame.
              (setq lhOk (fcd:v21-set-panel-lh-only obj lReq hReq))
              (vl-catch-all-apply 'vla-Update (list obj))
              (setq frame (if lhOk (fcd:v27-frame-from-origins obj) nil))
              (if frame (setq mode (fcd:v27-origin-mode obj frame)))
              (setq zero (if frame (fcd:v29-effective-zero-wcs obj mode) nil))

              ; Move the virtual frame so its effective zero lands on the
              ; internal anchor. The target contour remains inside the frame.
              (if zero
                (progn
                  (setq dx (- (car anchor) (car zero)))
                  (setq dy (- (cadr anchor) (cadr zero)))
                  (if (or (> (abs dx) 0.000001) (> (abs dy) 0.000001))
                    (progn
                      (fcd:v14-move-object obj dx dy)
                      (vl-catch-all-apply 'vla-Update (list obj))
                    )
                  )
                  (setq frame (fcd:v27-frame-from-origins obj))
                  (if frame (setq mode (fcd:v27-origin-mode obj frame)))
                )
              )
            )
          )

          (setq cutOk nil)
          (if (and plan frame zero)
            (setq cutOk (fcd:v30-set-frame-fit-props obj bbox cuts frame mode))
          )

          (if (and lhOk plan frame zero cutOk)
            (progn
              (vl-catch-all-apply 'vla-Update (list obj))
              obj
            )
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v30-insert-panel-shape (bbox cuts / obj)
  (setq obj (fcd:v27-insert-panel-shape bbox cuts))
  (if obj
    obj
    (fcd:v30-insert-panel-shape-anchor bbox cuts)
  )
)

(defun fcd:v17-insert-panel-shape (bbox cuts)
  (fcd:v30-insert-panel-shape bbox cuts)
)

(defun fcd:v29-panel-build-command (/ ss data shapes stats blockName)
  (fcd:init-layers)
  (setq blockName (fcd:v14-panel-block-name))

  (if (not (fcd:v14-block-exists-p))
    (fcd:msg "Block FCD_Panel not found in drawing.")
    (progn
      (fcd:delete-layer-objects "FCD_PANELS")
      (fcd:delete-layer-objects "FCD_PANEL_GUIDES")

      (fcd:msg "Select layout geometry. V30: V27 first, enlarged virtual frame fallback.")
      (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

      (if (not ss)
        (fcd:msg "No geometry selected.")
        (progn
          (setq data (fcd:v14-shapes-from-selection ss))
          (if (not (cdr (assoc 'ok data)))
            (fcd:msg "Not enough geometry: at least 2 X and 2 Y coordinates are required.")
            (progn
              (setq shapes (cdr (assoc 'shapes data)))
              (setq stats (fcd:v17-build-panel-blocks shapes))
              (fcd:msg
                (strcat
                  "Panel build complete V30. Inserted: " (itoa (cdr (assoc 'inserted stats)))
                  " [RECT: " (itoa (cdr (assoc 'insertedRect stats)))
                  ", L: " (itoa (cdr (assoc 'insertedL stats)))
                  ", T: " (itoa (cdr (assoc 'insertedT stats)))
                  "], SMALL: " (itoa (cdr (assoc 'skippedSmall stats)))
                  ", BIG: " (itoa (cdr (assoc 'skippedBig stats)))
                  ", U/P reserved: " (itoa (cdr (assoc 'skippedU stats)))
                  ", unsupported: " (itoa (cdr (assoc 'skippedUnsupported stats)))
                  ", cut calculation errors: " (itoa (cdr (assoc 'failedCuts stats)))
                  ", insert/dynamic errors: " (itoa (cdr (assoc 'failed stats)))
                  ", invalid: " (itoa (cdr (assoc 'invalid stats)))
                  ", service/preview skipped: " (itoa (cdr (assoc 'wrongLayer data)))
                  ", ignored objects/segments: " (itoa (cdr (assoc 'ignored data)))
                  "."
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun c:FCD_PANEL_BUILD ()
  (fcd:v29-panel-build-command)
)

(defun c:FCD_PANEL_BUILD_RECT ()
  (fcd:v29-panel-build-command)
)

(princ "\nFCD_CORE_V30 loaded: virtual frame fallback for predictable grips.")
(princ)

; ============================================================
; V30B - STABLE INSERTION FIRST, ALL-OR-NOTHING GRIP FIT
;
; Keep the successful V29 insertion behavior. Grip fitting is attempted after
; the block is already placed. Values are calculated as signed distances from
; the shifted dynamic frame, then written only if all eight handles are valid.
; ============================================================

(defun fcd:v30b-signed-cut-size (bbox cuts frameWcs key / target axis side val)
  (setq target (fcd:v29-target-edge bbox cuts key))
  (setq axis (fcd:v26-cut-key-axis key))
  (setq val nil)

  (if (and frameWcs (numberp target))
    (cond
      ((= axis 'X)
        (setq side (fcd:v26-cut-key-x-side key))
        (cond
          ((= side 'LEFT) (setq val (- target (nth 0 frameWcs))))
          ((= side 'RIGHT) (setq val (- (nth 2 frameWcs) target)))
        )
      )
      ((= axis 'Y)
        (setq side (fcd:v26-cut-key-y-side key))
        (cond
          ((= side 'TOP) (setq val (- (nth 3 frameWcs) target)))
          ((= side 'BOTTOM) (setq val (- target (nth 1 frameWcs))))
        )
      )
    )
  )

  val
)

(defun fcd:v30b-handle-plan (obj bbox cuts frame mode / frameWcs ok plan keys key cutSize handleValue)
  (setq frameWcs (fcd:v27-frame-wcs obj frame mode))
  (setq ok T)
  (setq plan nil)
  (setq keys (fcd:v23-cut-param-keys))

  (foreach key keys
    (setq cutSize (fcd:v30b-signed-cut-size bbox cuts frameWcs key))
    (setq handleValue
      (if (numberp cutSize)
        (fcd:v27-target-corner-handle-value obj key (* 1.0 cutSize) frame)
        nil
      )
    )

    (if (not handleValue)
      (setq ok nil)
      (setq plan (cons (cons key handleValue) plan))
    )
  )

  (if ok (reverse plan) nil)
)

(defun fcd:v30b-apply-handle-plan (obj plan / ok pair)
  (setq ok T)
  (foreach pair plan
    (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name (car pair)) (* 1.0 (cdr pair))))
      (setq ok nil)
    )
  )
  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v30b-fit-grips (obj bbox cuts frame mode / plan)
  (setq plan (fcd:v30b-handle-plan obj bbox cuts frame mode))
  (if plan
    (fcd:v30b-apply-handle-plan obj plan)
    nil
  )
)

(defun fcd:v30-insert-panel-shape-anchor (bbox cuts / x1 y1 x2 y2 w h blockName obj tempPt lhOk frame mode alignOk anchor zero dx dy cutOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      ; This is the V29 placement behavior: target-sized frame, aligned to the
      ; target bbox, then effective zero moved to the safe internal anchor.
      (setq anchor (fcd:v29-anchor-point bbox cuts))

      (if anchor
        (progn
          (setq tempPt (list x1 y1 0.0))
          (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

          (if obj
            (progn
              (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
              (fcd:v16-reset-block-scale obj)
              (fcd:v29-force-unit-scale obj)
              (fcd:v22-reset-block-if-possible obj)
              (fcd:v29-force-unit-scale obj)

              (setq lhOk (fcd:v21-set-panel-lh-only obj w h))
              (vl-catch-all-apply 'vla-Update (list obj))

              (setq frame nil)
              (setq mode 'LOCAL)
              (if lhOk
                (progn
                  (setq frame (fcd:v27-frame-from-origins obj))
                  (if frame (setq mode (fcd:v27-origin-mode obj frame)))
                )
              )

              (setq alignOk nil)
              (if frame
                (setq alignOk (fcd:v27-align-frame-min-to-rect obj frame mode x1 y1))
              )

              (setq frame nil)
              (setq zero nil)
              (if alignOk
                (progn
                  (setq frame (fcd:v27-frame-from-origins obj))
                  (if frame
                    (progn
                      (setq mode (fcd:v27-origin-mode obj frame))
                      (setq zero (fcd:v29-effective-zero-wcs obj mode))
                    )
                  )
                )
              )

              (if zero
                (progn
                  (setq dx (- (car anchor) (car zero)))
                  (setq dy (- (cadr anchor) (cadr zero)))
                  (if (or (> (abs dx) 0.000001) (> (abs dy) 0.000001))
                    (progn
                      (fcd:v14-move-object obj dx dy)
                      (vl-catch-all-apply 'vla-Update (list obj))
                    )
                  )
                  (setq frame (fcd:v27-frame-from-origins obj))
                  (if frame (setq mode (fcd:v27-origin-mode obj frame)))
                )
              )

              (setq cutOk nil)
              (if (and frame zero)
                (setq cutOk (fcd:v30b-fit-grips obj bbox cuts frame mode))
              )

              ; Stable insertion comes first. If grip fitting cannot be solved
              ; yet, keep the inserted block rather than deleting it.
              (if (and lhOk alignOk zero)
                (progn
                  (vl-catch-all-apply 'vla-Update (list obj))
                  obj
                )
                (progn
                  (fcd:v14-delete-object obj)
                  nil
                )
              )
            )
          )
        )
      )
    )
  )
)

(defun fcd:v30-insert-panel-shape (bbox cuts / obj)
  (setq obj (fcd:v27-insert-panel-shape bbox cuts))
  (if obj
    obj
    (fcd:v30-insert-panel-shape-anchor bbox cuts)
  )
)

(defun fcd:v17-insert-panel-shape (bbox cuts)
  (fcd:v30-insert-panel-shape bbox cuts)
)

(princ "\nFCD_CORE_V30 updated: V29 placement restored, signed all-or-nothing grip fitting.")
(princ)

; ============================================================
; V30C - HARD-PANEL CARRIER MODE
;
; Some FCD_Panel corner cases cannot be represented by the block's fixed
; neutral axes without crossing the dynamic zero. In those cases keep the
; stable zero placement, use a larger unscaled dynamic carrier when needed,
; set grips as direct distances to target edges, and clip the block to the
; calculated target contour. V27 is still used first for normal panels.
; ============================================================

(defun fcd:v30c-clamp (v lo hi)
  (max lo (min hi v))
)

(defun fcd:v30c-cut-active-p (cuts xKey yKey / w h)
  (setq w (fcd:v26-cut-size-for-key cuts xKey))
  (setq h (fcd:v26-cut-size-for-key cuts yKey))
  (fcd:v17-cut-active-p w h)
)

(defun fcd:v30c-target-intervals (bbox cuts / xLo xHi yLo yHi keys key axis side target)
  (setq xLo (nth 0 bbox))
  (setq yLo (nth 1 bbox))
  (setq xHi (nth 2 bbox))
  (setq yHi (nth 3 bbox))

  (setq keys (fcd:v23-cut-param-keys))
  (foreach key keys
    (setq target (fcd:v29-target-edge bbox cuts key))
    (setq axis (fcd:v26-cut-key-axis key))
    (if (numberp target)
      (cond
        ((= axis 'X)
          (setq side (fcd:v26-cut-key-x-side key))
          (cond
            ((= side 'LEFT) (setq xLo (max xLo target)))
            ((= side 'RIGHT) (setq xHi (min xHi target)))
          )
        )
        ((= axis 'Y)
          (setq side (fcd:v26-cut-key-y-side key))
          (cond
            ((= side 'BOTTOM) (setq yLo (max yLo target)))
            ((= side 'TOP) (setq yHi (min yHi target)))
          )
        )
      )
    )
  )

  (if (and (<= xLo xHi) (<= yLo yHi))
    (list (cons 'xLo xLo) (cons 'xHi xHi) (cons 'yLo yLo) (cons 'yHi yHi))
    nil
  )
)

(defun fcd:v30c-carrier-anchor (bbox cuts leftNeutral topNeutral / ints x1 y1 x2 y2 xLo xHi yLo yHi ax ay)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))

  (if (= (length (fcd:v30d-active-corners cuts)) 0)
    (list (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))
    (progn
      (setq ints (fcd:v30c-target-intervals bbox cuts))

      (if ints
        (progn
          (setq xLo (cdr (assoc 'xLo ints)))
          (setq xHi (cdr (assoc 'xHi ints)))
          (setq yLo (cdr (assoc 'yLo ints)))
          (setq yHi (cdr (assoc 'yHi ints)))

          ; Keep V30D's stable carrier placement: prefer the natural neutral
          ; axes that make the raw block frame cover the target bbox, but clamp
          ; them inside the valid polygon so cut handles do not cross zero.
          (setq ax (fcd:v30c-clamp (+ x1 leftNeutral) xLo xHi))
          (setq ay (fcd:v30c-clamp (- y2 topNeutral) yLo yHi))
          (list ax ay)
        )
        nil
      )
    )
  )
)

(defun fcd:v30c-carrier-size (bbox cuts anchor leftNeutral topNeutral / x1 y1 x2 y2 w h rightDist bottomDist lReq hReq)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))

  (if (= (length (fcd:v30d-active-corners cuts)) 0)
    (list w h)
    (progn
      (setq rightDist (max 0.0 (- x2 (car anchor))))
      (setq bottomDist (max 0.0 (- (cadr anchor) y1)))
      (setq lReq (+ leftNeutral rightDist))
      (setq hReq (+ topNeutral bottomDist))

      (list
        (max w lReq)
        (max h hReq)
      )
    )
  )
)

(defun fcd:v30c-neutral-data (obj frame mode / zero fw leftN topN)
  (setq zero (fcd:v29-effective-zero-wcs obj mode))
  (setq fw (fcd:v27-frame-wcs obj frame mode))

  (if (and zero fw)
    (progn
      (setq leftN (- (car zero) (nth 0 fw)))
      (setq topN (- (nth 3 fw) (cadr zero)))
      (if (and (numberp leftN) (numberp topN) (> leftN 0.001) (> topN 0.001))
        (list (cons 'zero zero) (cons 'left leftN) (cons 'top topN))
        nil
      )
    )
    nil
  )
)

(defun fcd:v30c-cut-rects (bbox cuts / x1 y1 x2 y2 w h rects)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq rects nil)

  (if (fcd:v30c-cut-active-p cuts 'LT_X 'LT_Y)
    (progn
      (setq w (fcd:v26-cut-size-for-key cuts 'LT_X))
      (setq h (fcd:v26-cut-size-for-key cuts 'LT_Y))
      (setq rects (cons (list x1 (- y2 h) (+ x1 w) y2) rects))
    )
  )

  (if (fcd:v30c-cut-active-p cuts 'RT_X 'RT_Y)
    (progn
      (setq w (fcd:v26-cut-size-for-key cuts 'RT_X))
      (setq h (fcd:v26-cut-size-for-key cuts 'RT_Y))
      (setq rects (cons (list (- x2 w) (- y2 h) x2 y2) rects))
    )
  )

  (if (fcd:v30c-cut-active-p cuts 'RB_X 'RB_Y)
    (progn
      (setq w (fcd:v26-cut-size-for-key cuts 'RB_X))
      (setq h (fcd:v26-cut-size-for-key cuts 'RB_Y))
      (setq rects (cons (list (- x2 w) y1 x2 (+ y1 h)) rects))
    )
  )

  (if (fcd:v30c-cut-active-p cuts 'LB_X 'LB_Y)
    (progn
      (setq w (fcd:v26-cut-size-for-key cuts 'LB_X))
      (setq h (fcd:v26-cut-size-for-key cuts 'LB_Y))
      (setq rects (cons (list x1 y1 (+ x1 w) (+ y1 h)) rects))
    )
  )

  rects
)

(defun fcd:v30c-point-in-cut-rect-p (x y rects / hit r tol)
  (setq hit nil)
  (setq tol (fcd:get 'tol))
  (foreach r rects
    (if (and
          (> x (+ (nth 0 r) tol))
          (< x (- (nth 2 r) tol))
          (> y (+ (nth 1 r) tol))
          (< y (- (nth 3 r) tol))
        )
      (setq hit T)
    )
  )
  hit
)

(defun fcd:v30c-cell-key (i j)
  (strcat (itoa i) ":" (itoa j))
)

(defun fcd:v30c-kept-cell-p (i j kept)
  (if (member (fcd:v30c-cell-key i j) kept) T nil)
)

(defun fcd:v30c-add-edge (edges p1 p2)
  (cons (list p1 p2) edges)
)

(defun fcd:v30c-order-edges (edges / start cur pts edge rest next guard tol)
  (setq tol (fcd:get 'tol))
  (setq edge (car edges))
  (setq edges (cdr edges))
  (setq start (car edge))
  (setq cur (cadr edge))
  (setq pts (list start cur))
  (setq guard 0)

  (while (and edges (not (equal cur start tol)) (< guard 200))
    (setq next nil)
    (setq rest nil)

    (foreach edge edges
      (if (and (not next) (equal (car edge) cur tol))
        (setq next edge)
        (setq rest (cons edge rest))
      )
    )

    (if next
      (progn
        (setq cur (cadr next))
        (setq pts (append pts (list cur)))
        (setq edges (reverse rest))
      )
      (setq edges nil)
    )

    (setq guard (1+ guard))
  )

  (if (and (> (length pts) 1) (equal (car pts) (car (last pts)) tol))
    (reverse (cdr (reverse pts)))
    pts
  )
)

(defun fcd:v30c-target-poly (bbox cuts / x1 y1 x2 y2 rects xs ys r i j xA xB yA yB cx cy kept cells cell edges pts)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq rects (fcd:v30c-cut-rects bbox cuts))

  (setq xs nil)
  (setq ys nil)
  (setq xs (fcd:add-unique-number x1 xs (fcd:get 'tol)))
  (setq xs (fcd:add-unique-number x2 xs (fcd:get 'tol)))
  (setq ys (fcd:add-unique-number y1 ys (fcd:get 'tol)))
  (setq ys (fcd:add-unique-number y2 ys (fcd:get 'tol)))

  (foreach r rects
    (setq xs (fcd:add-unique-number (nth 0 r) xs (fcd:get 'tol)))
    (setq xs (fcd:add-unique-number (nth 2 r) xs (fcd:get 'tol)))
    (setq ys (fcd:add-unique-number (nth 1 r) ys (fcd:get 'tol)))
    (setq ys (fcd:add-unique-number (nth 3 r) ys (fcd:get 'tol)))
  )

  (setq xs (fcd:sort-numbers xs))
  (setq ys (fcd:sort-numbers ys))
  (setq kept nil)
  (setq cells nil)

  (setq i 0)
  (while (< i (1- (length xs)))
    (setq xA (nth i xs))
    (setq xB (nth (1+ i) xs))
    (setq j 0)
    (while (< j (1- (length ys)))
      (setq yA (nth j ys))
      (setq yB (nth (1+ j) ys))
      (setq cx (/ (+ xA xB) 2.0))
      (setq cy (/ (+ yA yB) 2.0))

      (if (not (fcd:v30c-point-in-cut-rect-p cx cy rects))
        (progn
          (setq kept (cons (fcd:v30c-cell-key i j) kept))
          (setq cells (cons (list i j xA yA xB yB) cells))
        )
      )

      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  (setq edges nil)
  (foreach cell cells
    (setq i (nth 0 cell))
    (setq j (nth 1 cell))
    (setq xA (nth 2 cell))
    (setq yA (nth 3 cell))
    (setq xB (nth 4 cell))
    (setq yB (nth 5 cell))

    (if (not (fcd:v30c-kept-cell-p i (1- j) kept))
      (setq edges (fcd:v30c-add-edge edges (list xA yA) (list xB yA)))
    )
    (if (not (fcd:v30c-kept-cell-p (1+ i) j kept))
      (setq edges (fcd:v30c-add-edge edges (list xB yA) (list xB yB)))
    )
    (if (not (fcd:v30c-kept-cell-p i (1+ j) kept))
      (setq edges (fcd:v30c-add-edge edges (list xB yB) (list xA yB)))
    )
    (if (not (fcd:v30c-kept-cell-p (1- i) j kept))
      (setq edges (fcd:v30c-add-edge edges (list xA yB) (list xA yA)))
    )
  )

  (setq pts (if edges (fcd:v30c-order-edges edges) nil))
  (if (and pts (>= (length pts) 3)) pts nil)
)

(defun fcd:v30c-make-clip-polyline (pts / data p ent)
  (setq data
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 "FCD_PANEL_GUIDES")
      (cons 100 "AcDbPolyline")
      (cons 90 (length pts))
      (cons 70 1)
    )
  )

  (foreach p pts
    (setq data (append data (list (cons 10 (list (car p) (cadr p))))))
  )

  (setq ent (entmakex data))
  ent
)

(defun fcd:v30c-apply-xclip (obj pts / pl oldEcho res ok)
  (setq pl nil)
  (setq ok nil)
  (if (and obj pts (>= (length pts) 3))
    (progn
      (setq pl (fcd:v30c-make-clip-polyline pts))
      (if pl
        (progn
          (setq oldEcho (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (setq res
            (vl-catch-all-apply
              'vl-cmdf
              (list "_.XCLIP" (vlax-vla-object->ename obj) "" "_New" "_Select" pl)
            )
          )
          (setvar "CMDECHO" oldEcho)
          (setq ok (not (vl-catch-all-error-p res)))
          ; XCLIP stores its own spatial filter on the block reference. The
          ; source polyline is only a temporary construction aid.
          (entdel pl)
          ok
        )
        nil
      )
    )
    nil
  )
)

(defun fcd:v30c-bbox-near-target-p (obj bbox / b tol)
  (setq b (fcd:v14-bbox obj))
  (setq tol 25.0)
  (and
    b
    (<= (abs (- (fcd:v14-bbox-min-x b) (nth 0 bbox))) tol)
    (<= (abs (- (fcd:v14-bbox-min-y b) (nth 1 bbox))) tol)
    (<= (abs (- (fcd:v14-bbox-max-x b) (nth 2 bbox))) tol)
    (<= (abs (- (fcd:v14-bbox-max-y b) (nth 3 bbox))) tol)
  )
)

(defun fcd:v30d-corner-active-p (cuts corner / xKey yKey)
  (cond
    ((= corner 'LT) (setq xKey 'LT_X yKey 'LT_Y))
    ((= corner 'RT) (setq xKey 'RT_X yKey 'RT_Y))
    ((= corner 'RB) (setq xKey 'RB_X yKey 'RB_Y))
    ((= corner 'LB) (setq xKey 'LB_X yKey 'LB_Y))
  )
  (if (and xKey yKey)
    (fcd:v30c-cut-active-p cuts xKey yKey)
    nil
  )
)

(defun fcd:v30d-active-corners (cuts / result corner)
  (setq result nil)
  (foreach corner (list 'LT 'RT 'RB 'LB)
    (if (fcd:v30d-corner-active-p cuts corner)
      (setq result (cons corner result))
    )
  )
  (reverse result)
)

(defun fcd:v30d-corner-for-key (key / s)
  (setq s (vl-symbol-name key))
  (cond
    ((wcmatch s "LT_*") 'LT)
    ((wcmatch s "RT_*") 'RT)
    ((wcmatch s "RB_*") 'RB)
    ((wcmatch s "LB_*") 'LB)
    (T nil)
  )
)

(defun fcd:v30d-outer-edge (bbox key / axis side)
  (setq axis (fcd:v26-cut-key-axis key))
  (cond
    ((= axis 'X)
      (setq side (fcd:v26-cut-key-x-side key))
      (cond
        ((= side 'LEFT) (nth 0 bbox))
        ((= side 'RIGHT) (nth 2 bbox))
        (T nil)
      )
    )
    ((= axis 'Y)
      (setq side (fcd:v26-cut-key-y-side key))
      (cond
        ((= side 'TOP) (nth 3 bbox))
        ((= side 'BOTTOM) (nth 1 bbox))
        (T nil)
      )
    )
    (T nil)
  )
)

(defun fcd:v30d-axis-mode-for-key (key cornerModes / corner pair modes axis)
  (setq corner (fcd:v30d-corner-for-key key))
  (setq pair (if corner (assoc corner cornerModes) nil))
  (setq modes (if pair (cdr pair) nil))
  (setq axis (fcd:v26-cut-key-axis key))

  (cond
    ((not modes) 'INNER)
    ((= axis 'X) (car modes))
    ((= axis 'Y) (cadr modes))
    (T 'INNER)
  )
)

(defun fcd:v30d-target-edge (bbox cuts key cornerModes / mode)
  (setq mode (fcd:v30d-axis-mode-for-key key cornerModes))
  (if (= mode 'OUTER)
    (fcd:v30d-outer-edge bbox key)
    (fcd:v29-target-edge bbox cuts key)
  )
)

(defun fcd:v30d-direct-handle-value (obj bbox cuts key mode cornerModes / origin target axis side val)
  (setq origin (fcd:v29-origin-wcs obj key mode))
  (setq target (fcd:v30d-target-edge bbox cuts key cornerModes))
  (setq axis (fcd:v26-cut-key-axis key))
  (setq val nil)

  (if (and origin (numberp target))
    (cond
      ((= axis 'X)
        (setq side (fcd:v26-cut-key-x-side key))
        (cond
          ((= side 'LEFT) (setq val (- (car origin) target)))
          ((= side 'RIGHT) (setq val (- target (car origin))))
        )
      )
      ((= axis 'Y)
        (setq side (fcd:v26-cut-key-y-side key))
        (cond
          ((= side 'TOP) (setq val (- target (cadr origin))))
          ((= side 'BOTTOM) (setq val (- (cadr origin) target)))
        )
      )
    )
  )

  (fcd:v27-positive-handle val)
)

(defun fcd:v30d-handle-plan (obj bbox cuts mode cornerModes / ok keys key handleValue plan)
  (setq ok T)
  (setq plan nil)
  (setq keys (fcd:v23-cut-param-keys))

  (foreach key keys
    (setq handleValue (fcd:v30d-direct-handle-value obj bbox cuts key mode cornerModes))
    (if (not handleValue)
      (setq ok nil)
      (setq plan (cons (cons key handleValue) plan))
    )
  )

  (if ok (reverse plan) nil)
)

(defun fcd:v30d-apply-plan (obj plan / ok pair)
  (setq ok T)
  (foreach pair plan
    (if (not (fcd:v17-set-dynprop-exact obj (vl-symbol-name (car pair)) (* 1.0 (cdr pair))))
      (setq ok nil)
    )
  )
  (vl-catch-all-apply 'vla-Update (list obj))
  ok
)

(defun fcd:v30d-mode-options ()
  ; Direct fallback order: keep the V30D layout first because it leaves the
  ; least distorted grip pattern when a raw dynamic fit is impossible and XCLIP
  ; has to trim the carrier.
  (list
    (list 'INNER 'OUTER)
    (list 'OUTER 'INNER)
    (list 'OUTER 'OUTER)
    (list 'INNER 'INNER)
  )
)

(defun fcd:v30d-corner-mode-candidates (cuts / active opts result c1 c2 m1 m2)
  (setq active (fcd:v30d-active-corners cuts))
  (setq opts (fcd:v30d-mode-options))
  (setq result nil)

  (cond
    ((= (length active) 0)
      (setq result (list nil))
    )
    ((= (length active) 1)
      (setq c1 (car active))
      (foreach m1 opts
        (setq result (append result (list (list (cons c1 m1)))))
      )
    )
    (T
      (setq c1 (car active))
      (setq c2 (cadr active))
      (foreach m1 opts
        (foreach m2 opts
          (setq result (append result (list (list (cons c1 m1) (cons c2 m2)))))
        )
      )
    )
  )

  result
)

(defun fcd:v30f-v27like-plan (obj bbox cuts frame mode / plan)
  ; Reuse the stable V27 Origin/frame conversion against the enlarged fallback
  ; carrier. This often picks the visually correct opposite handle pair for
  ; T/L blocks whose local dynamic zero differs from the world cut corner.
  (if frame
    (fcd:v30b-handle-plan obj bbox cuts frame mode)
    nil
  )
)

(defun fcd:v30d-fit-grips (obj bbox cuts frame mode / candidates candidate plan firstPlan accepted)
  (setq candidates (fcd:v30d-corner-mode-candidates cuts))
  (setq firstPlan nil)
  (setq accepted nil)

  ; First try the V27-style grip mapping. It is the most predictable mapping
  ; for this block, but do not keep it as fallback unless the raw contour fits.
  (setq plan (fcd:v30f-v27like-plan obj bbox cuts frame mode))
  (if plan
    (progn
      (fcd:v30d-apply-plan obj plan)
      (if (fcd:v30c-bbox-near-target-p obj bbox)
        (setq accepted T)
      )
    )
  )

  (foreach candidate candidates
    (if (not accepted)
      (progn
        (setq plan (fcd:v30d-handle-plan obj bbox cuts mode candidate))
        (if plan
          (progn
            (if (not firstPlan) (setq firstPlan plan))
            (fcd:v30d-apply-plan obj plan)
            (if (fcd:v30c-bbox-near-target-p obj bbox)
              (setq accepted T)
            )
          )
        )
      )
    )
  )

  ; If none of the modes gives an exact raw dynamic shape, leave the most
  ; reasonable grip layout in place and let the XCLIP safety frame trim it.
  (if (and (not accepted) firstPlan)
    (fcd:v30d-apply-plan obj firstPlan)
  )

  accepted
)

(defun fcd:v30-insert-panel-shape-anchor (bbox cuts / x1 y1 x2 y2 w h blockName obj tempPt lhOk frame mode neutral leftN topN anchor carrier carrierL carrierH zero dx dy cutOk clipPts clipOk)
  (setq x1 (nth 0 bbox))
  (setq y1 (nth 1 bbox))
  (setq x2 (nth 2 bbox))
  (setq y2 (nth 3 bbox))
  (setq w (- x2 x1))
  (setq h (- y2 y1))
  (setq blockName (fcd:v14-panel-block-name))
  (setq obj nil)

  (if (and (> w (fcd:get 'tol)) (> h (fcd:get 'tol)))
    (progn
      (setq tempPt (list x1 y1 0.0))
      (setq obj (fcd:v14-insert-raw blockName tempPt 1.0 1.0))

      (if obj
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list obj "FCD_PANELS"))
          (fcd:v16-reset-block-scale obj)
          (fcd:v29-force-unit-scale obj)
          (fcd:v22-reset-block-if-possible obj)
          (fcd:v29-force-unit-scale obj)

          ; Read the block's neutral offsets from the real dynamic origins.
          (setq lhOk (fcd:v21-set-panel-lh-only obj w h))
          (vl-catch-all-apply 'vla-Update (list obj))
          (setq frame (if lhOk (fcd:v27-frame-from-origins obj) nil))
          (setq mode (if frame (fcd:v27-origin-mode obj frame) 'LOCAL))
          (setq neutral (if frame (fcd:v30c-neutral-data obj frame mode) nil))
          (setq leftN (cdr (assoc 'left neutral)))
          (setq topN (cdr (assoc 'top neutral)))
          (if (not (and (numberp leftN) (> leftN 0.001))) (setq leftN 427.02108))
          (if (not (and (numberp topN) (> topN 0.001))) (setq topN 229.749385))

          (setq anchor (fcd:v30c-carrier-anchor bbox cuts leftN topN))
          (setq carrier (if anchor (fcd:v30c-carrier-size bbox cuts anchor leftN topN) nil))
          (setq carrierL (if carrier (car carrier) w))
          (setq carrierH (if carrier (cadr carrier) h))

          ; Rebuild as an unscaled carrier large enough to cover the target.
          (setq lhOk (and anchor (fcd:v21-set-panel-lh-only obj carrierL carrierH)))
          (vl-catch-all-apply 'vla-Update (list obj))
          (setq frame (if lhOk (fcd:v27-frame-from-origins obj) nil))
          (setq mode (if frame (fcd:v27-origin-mode obj frame) 'LOCAL))
          (setq zero (if frame (fcd:v29-effective-zero-wcs obj mode) nil))

          (if zero
            (progn
              (setq dx (- (car anchor) (car zero)))
              (setq dy (- (cadr anchor) (cadr zero)))
              (if (or (> (abs dx) 0.000001) (> (abs dy) 0.000001))
                (progn
                  (fcd:v14-move-object obj dx dy)
                  (vl-catch-all-apply 'vla-Update (list obj))
                )
              )
              (setq frame (fcd:v27-frame-from-origins obj))
              (if frame (setq mode (fcd:v27-origin-mode obj frame)))
            )
          )

          (setq cutOk nil)
          (if (and frame zero)
            (setq cutOk (fcd:v30d-fit-grips obj bbox cuts frame mode))
          )

          (setq clipOk nil)
          (if (not cutOk)
            (progn
              (setq clipPts (fcd:v30c-target-poly bbox cuts))
              (setq clipOk (fcd:v30c-apply-xclip obj clipPts))
            )
          )

          (if (and lhOk zero)
            (progn
              (vl-catch-all-apply 'vla-Update (list obj))
              obj
            )
            (progn
              (fcd:v14-delete-object obj)
              nil
            )
          )
        )
      )
    )
  )
)

(defun fcd:v30-insert-panel-shape (bbox cuts / obj)
  (setq obj (fcd:v27-insert-panel-shape bbox cuts))

  (if (and obj (not (fcd:v30c-bbox-near-target-p obj bbox)))
    (progn
      (fcd:v14-delete-object obj)
      (setq obj nil)
    )
  )

  (if obj
    obj
    (fcd:v30-insert-panel-shape-anchor bbox cuts)
  )
)

(defun fcd:v17-insert-panel-shape (bbox cuts)
  (fcd:v30-insert-panel-shape bbox cuts)
)

(princ "\nFCD_CORE_V30H updated: V27-like exact fit only, stable V30D direct fallback grips.")
(princ)
