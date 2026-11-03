
    (function() {
      function parseDimension(dimension) {
        if (typeof dimension === "number") {
          return dimension;
        }

        if (typeof dimension !== "string") {
          return NaN;
        }

        const cleaned = dimension.replace(/\s+/g, "").replace(/,/g, ".");
        const parts = cleaned.split("+").map(function(part) {
          return part.replace(/[^\d.\-]/g, "");
        }).filter(function(part) {
          return part.length > 0;
        });

        if (parts.length === 0) {
          return NaN;
        }

        var sum = 0;

        for (var i = 0; i < parts.length; i++) {
          var n = parseFloat(parts[i]);

          if (!isFinite(n)) {
            return NaN;
          }

          sum += n;
        }

        return sum;
      }

      function getRectangleFixedSide(rectangles) {
        if (!rectangles || !rectangles.length) {
          return null;
        }

        const tolerance = 0.0001;
        const first = rectangles[0];
        const minSide = Math.min(first.height, first.width);
        const maxSide = Math.max(first.height, first.width);

        let allHaveSameMin = true;
        let allHaveSameMax = true;

        for (let i = 1; i < rectangles.length; i++) {
          const current = rectangles[i];
          const currentMin = Math.min(current.height, current.width);
          const currentMax = Math.max(current.height, current.width);

          if (Math.abs(currentMin - minSide) > tolerance) {
            allHaveSameMin = false;
          }

          if (Math.abs(currentMax - maxSide) > tolerance) {
            allHaveSameMax = false;
          }
        }

        if (allHaveSameMin) {
          return {
            fixedSide: minSide,
            fixedSideMode: "min"
          };
        }

        if (allHaveSameMax) {
          return {
            fixedSide: maxSide,
            fixedSideMode: "max"
          };
        }

        return null;
      }

      function getLinearLengthFromRectangle(rectangle, fixedInfo) {
        if (!fixedInfo) {
          return NaN;
        }

        if (fixedInfo.fixedSideMode === "min") {
          return Math.max(rectangle.height, rectangle.width);
        }

        if (fixedInfo.fixedSideMode === "max") {
          return Math.min(rectangle.height, rectangle.width);
        }

        return NaN;
      }

      let blocks = {};
      let layers = {};
      let fixedParts = {};
      let cuttingGroups = [];
      let idCounter = 1;
      let originalLines = [];

      const fileInput = document.getElementById("file-input");
      const fileLabel = document.getElementById("file-label");
      const output = document.getElementById("output");
      const progressBarFill = document.querySelector(".snake-progress-fill");
      const downloadButton = document.getElementById("download-button");
      const layerInputs = document.getElementById("layer-inputs");
      const markButton = document.getElementById("mark-button");
      const optimizeButton = document.getElementById("optimize-btn");
      const resetOptimizeButton = document.getElementById("reset-optimize-btn");
      const downloadPdfButton = document.getElementById("downloadPdf");
      const svgContainer = document.getElementById("svgContainer");
      const jsonProgressContainer = document.getElementById("jsonProgressContainer");
      const jsonProgressBar = document.getElementById("jsonProgressBar");
      const oversizedBlock = document.getElementById("oversized-block");
      const oversizedTableBody = document.getElementById("oversized-table-body");
      const oversizedEmpty = document.getElementById("oversized-empty");

      function getSafeId(layerName) {
        return Array.from(layerName).map(c => c.codePointAt(0).toString(16)).join("-");
      }

      function clearOversizedTable() {
        oversizedTableBody.innerHTML = "";
        oversizedBlock.classList.add("hidden");
        oversizedEmpty.style.display = "block";
      }

      function renderOversizedTable(items) {
        oversizedTableBody.innerHTML = "";

        if (!items || !items.length) {
          oversizedBlock.classList.add("hidden");
          oversizedEmpty.style.display = "block";
          return;
        }

        for (let i = 0; i < items.length; i++) {
          const item = items[i];
          const row = document.createElement("tr");

          const numberCell = document.createElement("td");
          numberCell.textContent = String(i + 1);
          row.appendChild(numberCell);

          const labelCell = document.createElement("td");
          labelCell.textContent = item.label || "";
          row.appendChild(labelCell);

          const materialCell = document.createElement("td");
          materialCell.textContent = item.material || "";
          row.appendChild(materialCell);

          const colorCell = document.createElement("td");
          colorCell.textContent = item.color || "";
          row.appendChild(colorCell);

          const typeCell = document.createElement("td");
          typeCell.textContent = item.type || "";
          row.appendChild(typeCell);

          const elementSizeCell = document.createElement("td");
          elementSizeCell.textContent = item.elementSize || "";
          row.appendChild(elementSizeCell);

          const stockSizeCell = document.createElement("td");
          stockSizeCell.textContent = item.stockSize || "";
          row.appendChild(stockSizeCell);

          const layerCell = document.createElement("td");
          layerCell.textContent = item.layer || "";
          row.appendChild(layerCell);

          const blockCell = document.createElement("td");
          blockCell.textContent = item.block || "";
          row.appendChild(blockCell);

          oversizedTableBody.appendChild(row);
        }

        oversizedBlock.classList.remove("hidden");
        oversizedEmpty.style.display = "none";
      }

      function resetCuttingResults() {
        svgContainer.innerHTML = "";
        jsonProgressContainer.classList.remove("visible");
        jsonProgressBar.style.width = "0%";
        jsonProgressBar.innerText = "0%";
        downloadPdfButton.disabled = true;
        clearOversizedTable();
        CuttingTable.resetCutRows();
      }

      function resetAllStateForNewFile() {
        output.innerText = "";
        progressBarFill.style.width = "0%";
        downloadButton.style.display = "none";
        markButton.style.display = "none";
        layerInputs.innerHTML = "";
        blocks = {};
        layers = {};
        fixedParts = {};
        cuttingGroups = [];
        idCounter = 1;
        originalLines = [];
        resetCuttingResults();
        CuttingTable.clear();
      }

      function parseSizeAttribute(value, ctx) {
        const contextInfo = ctx ? (" (layer: " + ctx.layer + ", block: " + ctx.block + ")") : "";
        const separators = ["x", "X", "х", "Х", "*", "×"];
        let separatorUsed = null;

        for (let s = 0; s < separators.length; s++) {
          if (String(value).includes(separators[s])) {
            separatorUsed = separators[s];
            break;
          }
        }

        if (separatorUsed) {
          const parts = String(value).split(separatorUsed).map(function(part) {
            return part.trim();
          });

          let height = null;
          let width = null;

          if (parts.length === 2) {
            const h = parseDimension(parts[0]);
            const w = parseDimension(parts[1]);
            height = h;
            width = w;
          }

          if (!isFinite(height) || !isFinite(width)) {
            console.error("❌ Не удалось распарсить прямоугольный размер \"" + value + "\"" + contextInfo);
          } else {
            console.log("✅ Прямоугольный размер \"" + value + "\" => H: " + height + " W: " + width + contextInfo);
          }

          return { height: height, width: width };
        } else {
          const widthOnly = parseDimension(String(value));

          if (!isFinite(widthOnly)) {
            console.error("❌ Не удалось распарсить линейный размер \"" + value + "\"" + contextInfo);
          } else {
            console.log("✅ Линейный размер \"" + value + "\" => L: " + widthOnly + contextInfo);
          }

          return { height: null, width: widthOnly };
        }
      }

      function generateUniqueId(prefix, counter) {
        return prefix + String(counter);
      }

      function displayLayerInputs() {
        layerInputs.innerHTML = "";

        for (const layer in layers) {
          if (layers[layer].blockNames.length > 0) {
            const safeLayer = getSafeId(layer);
            const layerDiv = document.createElement("div");
            layerDiv.classList.add("layer-item");

            const headerDiv = document.createElement("div");
            headerDiv.classList.add("layer-header");

            const label = document.createElement("label");
            label.innerText = layer;
            headerDiv.appendChild(label);

            let hasMarkirovka = false;
            let hasRazmer = false;
            let allLinear = true;

            for (const blockName of layers[layer].blockNames) {
              const block = blocks[layer][blockName];

              for (const attr of block.attributes) {
                if (attr.tag.toLowerCase() === "маркировка") {
                  hasMarkirovka = true;
                }

                if (attr.tag.toLowerCase() === "размер") {
                  hasRazmer = true;
                  const size = parseSizeAttribute(attr.value, { layer: layer, block: blockName });

                  if (isFinite(size.height) && size.height !== null) {
                    allLinear = false;
                  }

                  if (!isFinite(size.width)) {
                    allLinear = false;
                  }
                }
              }
            }

            layers[layer].hasLinearBlocks = allLinear;
            console.log("Layer \"" + layer + "\" hasLinearBlocks: " + layers[layer].hasLinearBlocks);

            if (hasMarkirovka) {
              const fixedPartInput = document.createElement("input");
              fixedPartInput.type = "text";
              fixedPartInput.placeholder = "Префикс маркировки блоков";
              fixedPartInput.id = "fixed-part-" + safeLayer;
              fixedPartInput.title = "Введите фиксированную часть маркировки, например \"КМ-\".";
              headerDiv.appendChild(fixedPartInput);
            }

            if (hasRazmer) {
              const checkboxLabel = document.createElement("label");
              const checkbox = document.createElement("input");
              checkbox.type = "checkbox";
              checkbox.id = "checkbox-" + safeLayer;

              checkbox.addEventListener("change", function(event) {
                const extraFields = layerDiv.querySelector(".extra-fields");

                if (event.target.checked) {
                  extraFields.classList.add("open");

                  if (layers[layer].hasLinearBlocks) {
                    extraFields.querySelector("#h-list-" + safeLayer).style.display = "none";
                    extraFields.querySelector("#vert-padding-" + safeLayer).style.display = "none";
                    extraFields.querySelector("#horz-padding-" + safeLayer).style.display = "none";
                    extraFields.querySelector("#rotate-" + safeLayer).style.display = "none";
                    extraFields.querySelector("#convert-to-bars-" + safeLayer).style.display = "none";
                    extraFields.querySelector("#l-list-" + safeLayer).placeholder = "L палки";
                  } else {
                    extraFields.querySelector("#h-list-" + safeLayer).style.display = "block";
                    extraFields.querySelector("#vert-padding-" + safeLayer).style.display = "block";
                    extraFields.querySelector("#horz-padding-" + safeLayer).style.display = "block";
                    extraFields.querySelector("#rotate-" + safeLayer).style.display = "inline-block";
                    extraFields.querySelector("#convert-to-bars-" + safeLayer).style.display = "flex";
                    extraFields.querySelector("#l-list-" + safeLayer).placeholder = "L листа";
                  }
                } else {
                  extraFields.classList.remove("open");
                }
              });

              checkboxLabel.innerText = "Раскрой";
              checkboxLabel.prepend(checkbox);
              headerDiv.appendChild(checkboxLabel);
            }

            layerDiv.appendChild(headerDiv);

            const extraFields = document.createElement("div");
            extraFields.classList.add("extra-fields");

            const hListInput = document.createElement("input");
            hListInput.type = "number";
            hListInput.placeholder = "H листа";
            hListInput.id = "h-list-" + safeLayer;
            extraFields.appendChild(hListInput);

            const lListInput = document.createElement("input");
            lListInput.type = "number";
            lListInput.placeholder = "L листа";
            lListInput.id = "l-list-" + safeLayer;
            extraFields.appendChild(lListInput);

            const vertPaddingInput = document.createElement("input");
            vertPaddingInput.type = "number";
            vertPaddingInput.placeholder = "В. припуск";
            vertPaddingInput.id = "vert-padding-" + safeLayer;
            vertPaddingInput.title = "Сумма развертки без учёта высоты";
            extraFields.appendChild(vertPaddingInput);

            const horzPaddingInput = document.createElement("input");
            horzPaddingInput.type = "number";
            horzPaddingInput.placeholder = "Г. припуск";
            horzPaddingInput.id = "horz-padding-" + safeLayer;
            horzPaddingInput.title = "Сумма развертки без учёта ширины";
            extraFields.appendChild(horzPaddingInput);

            const sawWidthInput = document.createElement("input");
            sawWidthInput.type = "number";
            sawWidthInput.placeholder = "Толщина пила";
            sawWidthInput.id = "saw-width-" + safeLayer;
            extraFields.appendChild(sawWidthInput);

            const rotateCheckbox = document.createElement("button");
            rotateCheckbox.classList.add("rotate-checkbox", "inactive");
            rotateCheckbox.id = "rotate-" + safeLayer;
            rotateCheckbox.title = "Поворот элемента внутри листа";
            rotateCheckbox.addEventListener("click", function() {
              rotateCheckbox.classList.toggle("active");
              rotateCheckbox.classList.toggle("inactive");
            });
            extraFields.appendChild(rotateCheckbox);

            const convertBarsContainer = document.createElement("div");
            convertBarsContainer.classList.add("convert-bars-container");
            convertBarsContainer.id = "convert-to-bars-" + safeLayer;

            if (layers[layer].hasLinearBlocks) {
              convertBarsContainer.style.display = "none";
            }

            const convertBarsCheckbox = document.createElement("input");
            convertBarsCheckbox.type = "checkbox";
            convertBarsCheckbox.id = "convert-to-bars-checkbox-" + safeLayer;
            convertBarsCheckbox.title = "Для прямоугольных элементов: ищет постоянную сторону и отправляет слой в раскрой как палки";

            const convertBarsLabel = document.createElement("label");
            convertBarsLabel.setAttribute("for", "convert-to-bars-checkbox-" + safeLayer);
            convertBarsLabel.innerText = "Перевести в палки";

            convertBarsContainer.appendChild(convertBarsCheckbox);
            convertBarsContainer.appendChild(convertBarsLabel);
            extraFields.appendChild(convertBarsContainer);

            if (layers[layer].hasLinearBlocks) {
              const angularContainer = document.createElement("div");
              angularContainer.classList.add("angular-container");

              const angularCheckbox = document.createElement("input");
              angularCheckbox.type = "checkbox";
              angularCheckbox.id = "angular-checkbox-" + safeLayer;
              angularCheckbox.title = "Учитывает двойную отрисовку профиля";

              const angularLabel = document.createElement("label");
              angularLabel.setAttribute("for", "angular-checkbox-" + safeLayer);
              angularLabel.innerText = "угловой";

              angularContainer.appendChild(angularCheckbox);
              angularContainer.appendChild(angularLabel);
              extraFields.appendChild(angularContainer);
            } else {
              const angularContainer = document.createElement("div");
              angularContainer.classList.add("angular-container");
              angularContainer.id = "angular-container-" + safeLayer;
              angularContainer.style.display = "none";

              const angularCheckbox = document.createElement("input");
              angularCheckbox.type = "checkbox";
              angularCheckbox.id = "angular-checkbox-" + safeLayer;
              angularCheckbox.title = "Учитывает двойную отрисовку профиля";

              const angularLabel = document.createElement("label");
              angularLabel.setAttribute("for", "angular-checkbox-" + safeLayer);
              angularLabel.innerText = "угловой";

              angularContainer.appendChild(angularCheckbox);
              angularContainer.appendChild(angularLabel);
              extraFields.appendChild(angularContainer);

              convertBarsCheckbox.addEventListener("change", function() {
                if (convertBarsCheckbox.checked) {
                  angularContainer.style.display = "flex";
                  hListInput.style.display = "none";
                  vertPaddingInput.style.display = "none";
                  horzPaddingInput.style.display = "none";
                  rotateCheckbox.style.display = "none";
                  lListInput.placeholder = "L палки";
                } else {
                  angularContainer.style.display = "none";
                  hListInput.style.display = "block";
                  vertPaddingInput.style.display = "block";
                  horzPaddingInput.style.display = "block";
                  rotateCheckbox.style.display = "inline-block";
                  lListInput.placeholder = "L листа";
                }
              });
            }

            layerDiv.appendChild(extraFields);
            layerInputs.appendChild(layerDiv);
          }
        }

        markButton.style.display = "block";
      }

      function updateBlocksWithIds() {
        idCounter = 1;
        const layerCounters = {};
        const uniqueCombinations = {};

        for (const layer in layers) {
          const safeLayer = getSafeId(layer);
          const inputElement = document.getElementById("fixed-part-" + safeLayer);

          if (inputElement) {
            const prefix = inputElement.value;
            fixedParts[layer] = prefix;
            layerCounters[layer] = 1;
            uniqueCombinations[layer] = {};
          }
        }

        for (const layer in blocks) {
          if (!uniqueCombinations[layer]) {
            continue;
          }

          for (const blockName in blocks[layer]) {
            const block = blocks[layer][blockName];
            let combinationKey = "";

            combinationKey = JSON.stringify(block.attributes.filter(attr => !['маркировка','id'].includes(attr.tag.toLowerCase())).map(attr => [attr.tag.toLowerCase(),attr.value]).sort((a,b) => a[0].localeCompare(b[0])));

            if (!(combinationKey in uniqueCombinations[layer])) {
              uniqueCombinations[layer][combinationKey] = generateUniqueId(fixedParts[layer], layerCounters[layer]);
              layerCounters[layer]++;
            }

            block.attributes.forEach(function(attr) {
              if (attr.tag.toLowerCase() === "маркировка") {
                attr.value = uniqueCombinations[layer][combinationKey];
              }

              if (attr.tag.toLowerCase() === "id") {
                attr.value = generateUniqueId("ID", idCounter++);
              }
            });
          }
        }
      }

      function updateSpecification() {
        const rows = [];
        for (const [material, layer] of Object.entries(layers)) {
          const groups = new Map();
          for (const key of new Set(layer.blockNames)) {
            const attrs = blocks[material][key].attributes;
            const value = attrs.find(a => a.tag.toLowerCase() === 'размер')?.value;
            if (!value) continue;
            const size = parseSizeAttribute(value);
            const color = attrs.find(a => a.tag.toLowerCase() === 'цвет')?.value || '';
            const isSheet = size.height !== null && size.height > 0 && size.width > 0;
            if (!isSheet && !(size.height === null && size.width > 0)) continue;
            const unit = isSheet ? 'м²' : 'м.пог';
            const keyGroup = JSON.stringify([color, unit]);
            const row = groups.get(keyGroup) || {material,name:material,label:'',color,unit,group:isSheet?'Облицовочные материалы':'Подоблицовочный металлокаркас',projectQty:0,size:0};
            row.projectQty += isSheet ? size.height * size.width / 1000000 : size.width / 1000;
            groups.set(keyGroup, row);
          }
          rows.push(...groups.values());
          if (layer.blocksWithoutAttrs) rows.push({material,name:material,label:'',unit:'шт',group:'Блоки без размеров',projectQty:layer.blocksWithoutAttrs});
          if (!layer.blockNames.length && !layer.blocksWithoutAttrs && layer.length>0) rows.push({material,name:material,label:'',unit:'м.пог',group:'Геометрия чертежа',projectQty:layer.length});
        }
        CuttingTable.setDxfRows(rows);
      }

      function buildCuttingGroups() {
        const optimizerData = [];
        const elementIdCounter = {};
        const processedBlocks = new Set();

        for (const layer in layers) {
          const safeLayer = getSafeId(layer);

          if (document.getElementById("checkbox-" + safeLayer) && document.getElementById("checkbox-" + safeLayer).checked) {
            const convertToBarsCheckbox = document.getElementById("convert-to-bars-checkbox-" + safeLayer);
            const shouldConvertRectanglesToBars = convertToBarsCheckbox ? convertToBarsCheckbox.checked : false;
            const baseIsLinearBlock = layers[layer].hasLinearBlocks;
            const isLinearBlock = baseIsLinearBlock || shouldConvertRectanglesToBars;
            const sheetWidthRaw = document.getElementById("l-list-" + safeLayer).value;
            const sheetWidth = parseDimension(sheetWidthRaw);

            let sheetHeight = 0;
            let verticalPadding = 0;
            let horizontalPadding = 0;
            let rotation = 0;

            if (!baseIsLinearBlock && !shouldConvertRectanglesToBars) {
              const sheetHeightRaw = document.getElementById("h-list-" + safeLayer).value;
              sheetHeight = parseDimension(sheetHeightRaw);
              verticalPadding = parseDimension(document.getElementById("vert-padding-" + safeLayer).value || "0");
              horizontalPadding = parseDimension(document.getElementById("horz-padding-" + safeLayer).value || "0");
              rotation = document.getElementById("rotate-" + safeLayer).classList.contains("active") ? 1 : 0;
            }

            const sawWidth = parseDimension(document.getElementById("saw-width-" + safeLayer).value || "0");
            const elements = [];
            elementIdCounter[layer] = elementIdCounter[layer] || 1;

            const rectangleElementsForConversion = [];

            for (const blockName of layers[layer].blockNames) {
              const block = blocks[layer][blockName];
              const blockKey = layer + "-" + blockName + "-" + JSON.stringify(block.attributes);

              if (processedBlocks.has(blockKey)) {
                continue;
              }

              processedBlocks.add(blockKey);

              for (const attr of block.attributes) {
                if (attr.tag.toLowerCase() === "размер") {
                  const size = parseSizeAttribute(attr.value, { layer: layer, block: blockName });

                  const commonElementData = {
                    material: layer,
                    color: "",
                    label: "",
                    id: "",
                    _raw: attr.value,
                    _block: blockName,
                    _layer: layer
                  };

                  block.attributes.forEach(function(a) {
                    if (a.tag.toLowerCase() === "цвет") {
                      commonElementData.color = a.value;
                    } else if (a.tag.toLowerCase() === "маркировка") {
                      commonElementData.label = a.value;
                    }
                  });

                  if (shouldConvertRectanglesToBars) {
                    if (isFinite(size.height) && isFinite(size.width) && size.height !== null && size.width !== null) {
                      rectangleElementsForConversion.push({
                        material: commonElementData.material,
                        color: commonElementData.color,
                        label: commonElementData.label,
                        id: "",
                        _raw: commonElementData._raw,
                        _block: commonElementData._block,
                        _layer: commonElementData._layer,
                        height: size.height,
                        width: size.width
                      });
                    } else {
                      console.error("❌ Для режима \"Перевести в палки\" найден не прямоугольный элемент: \"" + attr.value + "\" (layer: " + layer + ", block: " + blockName + ").");
                    }
                  } else if (isFinite(size.height) && isFinite(size.width) && size.height !== null && size.width !== null) {
                    const element = Object.assign({}, commonElementData);
                    element.height = size.height + (isFinite(verticalPadding) ? verticalPadding : 0);
                    element.width = size.width + (isFinite(horizontalPadding) ? horizontalPadding : 0);
                    element.id = generateUniqueId("ID", elementIdCounter[layer]);
                    elementIdCounter[layer]++;

                    if (isFinite(element.height) && isFinite(element.width)) {
                      elements.push(element);
                    } else {
                      console.error("❌ Прямоугольный элемент дал NaN после припусков: \"" + element._raw + "\" (layer: " + element._layer + ", block: " + element._block + ")");
                    }
                  } else if (size.height === null && isFinite(size.width)) {
                    const element = Object.assign({}, commonElementData);
                    element.length = size.width;
                    element.id = generateUniqueId("ID", elementIdCounter[layer]);
                    elementIdCounter[layer]++;

                    if (isFinite(element.length)) {
                      elements.push(element);
                    } else {
                      console.error("❌ Линейный элемент дал NaN длину: \"" + element._raw + "\" (layer: " + element._layer + ", block: " + element._block + ")");
                    }
                  } else {
                    console.error("❌ Атрибут РАЗМЕР не распознан: \"" + attr.value + "\" (layer: " + layer + ", block: " + blockName + ")");
                  }
                }
              }
            }

            if (shouldConvertRectanglesToBars) {
              if (!rectangleElementsForConversion.length) {
                console.warn("⚠️ Для слоя \"" + layer + "\" включен режим \"Перевести в палки\", но прямоугольные элементы не найдены. Группа будет пропущена.");
                continue;
              }

              const fixedInfo = getRectangleFixedSide(rectangleElementsForConversion);

              if (!fixedInfo || !isFinite(fixedInfo.fixedSide)) {
                console.error("❌ Слой \"" + layer + "\" нельзя перевести в палки: не удалось определить постоянную сторону профиля.");
                continue;
              }

              console.log("✅ Слой \"" + layer + "\" переведен в палки. Постоянная сторона: " + fixedInfo.fixedSide + " мм, режим: " + fixedInfo.fixedSideMode);

              for (let i = 0; i < rectangleElementsForConversion.length; i++) {
                const rectangle = rectangleElementsForConversion[i];
                const convertedLength = getLinearLengthFromRectangle(rectangle, fixedInfo);

                if (!isFinite(convertedLength) || convertedLength <= 0) {
                  console.error("❌ Не удалось вычислить длину палки для элемента \"" + rectangle._raw + "\" (layer: " + rectangle._layer + ", block: " + rectangle._block + ").");
                  continue;
                }

                const element = {
                  material: rectangle.material,
                  color: rectangle.color,
                  label: rectangle.label,
                  id: generateUniqueId("ID", elementIdCounter[layer]),
                  _raw: rectangle._raw,
                  _block: rectangle._block,
                  _layer: rectangle._layer,
                  length: convertedLength,
                  profileWidth: fixedInfo.fixedSide
                };

                elementIdCounter[layer]++;
                elements.push(element);
              }
            }

            const angularChecked = document.getElementById("angular-checkbox-" + safeLayer)
              ? document.getElementById("angular-checkbox-" + safeLayer).checked
              : false;

            let finalElements = elements.slice();

            if (angularChecked && isLinearBlock) {
              const sizeMap = new Map();

              finalElements = finalElements.filter(function(el) {
                const key = el.length;

                if (!sizeMap.has(key)) {
                  sizeMap.set(key, 1);
                  return true;
                } else {
                  const count = sizeMap.get(key);

                  if (count % 2 === 1) {
                    sizeMap.set(key, count + 1);
                    return false;
                  } else {
                    sizeMap.set(key, count + 1);
                    return true;
                  }
                }
              });
            }

            if (isLinearBlock) {
              if (!isFinite(sheetWidth) || sheetWidth <= 0) {
                console.error("❌ Неверная длина прутка (L палки): \"" + sheetWidthRaw + "\" для слоя \"" + layer + "\". Группа будет пропущена.");
                continue;
              }
            } else {
              const sheetHeightRaw = document.getElementById("h-list-" + safeLayer).value;

              if (!isFinite(sheetWidth) || sheetWidth <= 0 || !isFinite(sheetHeight) || sheetHeight <= 0) {
                console.error("❌ Неверный размер листа (L/H): L=\"" + document.getElementById("l-list-" + safeLayer).value + "\" H=\"" + sheetHeightRaw + "\" для слоя \"" + layer + "\". Группа будет пропущена.");
                continue;
              }
            }

            if (!finalElements.length) {
              console.warn("⚠️ В слое \"" + layer + "\" после фильтрации не осталось элементов для раскроя. Группа будет пропущена.");
              continue;
            }

            const colors = new Map();
            for (const element of finalElements) {
              if (!colors.has(element.color)) colors.set(element.color, []);
              colors.get(element.color).push(element);
            }
            for (const elements of colors.values()) optimizerData.push({sheetWidth,sheetHeight,sawWidth,rotation,isLinear:isLinearBlock,elements});
          }
        }

        cuttingGroups = optimizerData;
        console.log("JSON для Оптимизатора:", JSON.stringify(cuttingGroups, null, 2));
      }

      function updateDXFContent() {
        let updatedLines = [...originalLines];

        for (const layer in blocks) {
          for (const blockName in blocks[layer]) {
            const block = blocks[layer][blockName];

            block.attributes.forEach(function(attr, index) {
              if (attr.tag.toLowerCase() === "маркировка" || attr.tag.toLowerCase() === "id") {
                const startIndex = block.attribIndex[index];

                for (let i = startIndex; i < updatedLines.length; i++) {
                  if (updatedLines[i].trim() === "1") {
                    updatedLines[i + 1] = attr.value;
                    break;
                  }
                }
              }
            });
          }
        }

        return updatedLines.join("\n");
      }

      function displayResults(updatedContent) {
        output.style.display = "none";

        downloadButton.onclick = function() {
          const blob = new Blob([updatedContent], { type: "application/dxf" });
          const link = document.createElement("a");
          link.href = URL.createObjectURL(blob);
          link.download = "updated.dxf";
          document.body.appendChild(link);link.click();link.remove();
          setTimeout(()=>URL.revokeObjectURL(link.href),1000);
        };
      }

      const cuttingStatus = document.getElementById('cutting-status');
      function setBusy(busy) {
        fileInput.disabled = busy; markButton.disabled = busy; resetOptimizeButton.disabled = busy;
        optimizeButton.disabled = busy || !originalLines.length;
        layerInputs.querySelectorAll('input,button').forEach(control => control.disabled = busy);
      }
      optimizeButton.disabled = true;
      fileInput.accept = '.dxf';
      fileInput.addEventListener('change', async function(event) {
        const file = event.target.files[0];
        resetAllStateForNewFile();
        if (!file) {fileLabel.textContent='Выберите DXF-файл';setBusy(false);return;}
        setBusy(true);fileLabel.textContent=file.name;cuttingStatus.textContent='Чтение DXF…';
        try {
          const bytes = await file.arrayBuffer();
          let text;
          try {text=new TextDecoder('utf-8',{fatal:true}).decode(bytes);}
          catch {text=new TextDecoder('windows-1251').decode(bytes);}
          if (text.includes('AutoCAD Binary DXF')) throw new Error('Требуется текстовый DXF, а не бинарный.');
          const parsed=CuttingData.parseDxf(text);
          originalLines=parsed.lines;blocks=parsed.blocks;layers=parsed.layers;
          if (!Object.keys(layers).length) throw new Error('В разделе ENTITIES не найдены блоки или линейная геометрия.');
          displayLayerInputs();updateBlocksWithIds();updateSpecification();
          displayResults(updateDXFContent());downloadButton.style.display='block';
          progressBarFill.style.width='100%';cuttingStatus.textContent='DXF загружен. Настройте раскрой для нужных слоёв.';
        } catch(error) {originalLines=[];cuttingStatus.textContent=error.message;}
        finally {setBusy(false);}
      });
      markButton.onclick = function() {
        resetCuttingResults();updateBlocksWithIds();updateSpecification();displayResults(updateDXFContent());
        downloadButton.style.display='block';cuttingStatus.textContent='Маркировка обновлена в DXF. Для новой карты запустите раскрой.';
      };
      optimizeButton.onclick = async function() {
        const selected = [...layerInputs.querySelectorAll('[id^="checkbox-"]')].filter(c=>c.checked);
        if (!selected.length) {cuttingStatus.textContent='Включите раскрой хотя бы для одного слоя.';return;}
        for (const checkbox of selected) {
          const key=checkbox.id.slice('checkbox-'.length),get=id=>document.getElementById(id+'-'+key);
          const linear=get('convert-to-bars-checkbox')?.checked || get('h-list').style.display==='none';
          if (!(parseDimension(get('l-list').value)>0) || (!linear&&!(parseDimension(get('h-list').value)>0))) {cuttingStatus.textContent='Укажите положительные размеры заготовки для каждого выбранного слоя.';return;}
          if (['saw-width','vert-padding','horz-padding'].some(id=>get(id)&&parseDimension(get(id).value||'0')<0)) {cuttingStatus.textContent='Припуски и ширина пропила не могут быть отрицательными.';return;}
        }
        resetCuttingResults();updateBlocksWithIds();buildCuttingGroups();
        if(!cuttingGroups.length){cuttingStatus.textContent='Нет подходящих деталей. Проверьте размеры и параметры слоёв.';return;}
        setBusy(true);cuttingStatus.textContent='Выполняется раскрой…';
        try {
          await handleJsonData(cuttingGroups);
          displayResults(updateDXFContent());downloadButton.style.display='block';
          cuttingStatus.textContent='Раскрой завершён. Спецификация и КП обновлены. Проверьте список неразмещённых элементов.';
        } catch(error) {cuttingStatus.textContent='Ошибка раскроя: '+error.message;}
        finally {setBusy(false);}
      };
      resetOptimizeButton.onclick=function(){resetCuttingResults();cuttingStatus.textContent='Карта сброшена. В таблице восстановлены количества из DXF.';};

      async function handleJsonData(dataGroups) {
        const RALColors = {
          "RAL 1000": "#BEBD7F", "RAL 1001": "#C2B078", "RAL 1002": "#C6A664", "RAL 1003": "#E5BE01",
          "RAL 1004": "#CDA434", "RAL 1005": "#A98307", "RAL 1006": "#E4A010", "RAL 1007": "#DC9D00",
          "RAL 1011": "#8A6642", "RAL 1012": "#C7B446", "RAL 1013": "#EAE6CA", "RAL 1014": "#E1CC4F",
          "RAL 1015": "#EAE0D5", "RAL 1016": "#EDFF21", "RAL 1017": "#F5D033", "RAL 1018": "#F8F32B",
          "RAL 1019": "#9E9764", "RAL 1020": "#999950", "RAL 1021": "#F3DA0B", "RAL 1023": "#F7BA0B",
          "RAL 1024": "#AEA04B", "RAL 1026": "#FFFF00", "RAL 1027": "#9D9101", "RAL 1028": "#F4A900",
          "RAL 1032": "#D6AE01", "RAL 1033": "#F3A505", "RAL 1034": "#EFA94A", "RAL 1035": "#6A5D4D",
          "RAL 1036": "#705335", "RAL 1037": "#F39F18", "RAL 2000": "#ED760E", "RAL 2001": "#C4463D",
          "RAL 2002": "#A52019", "RAL 2003": "#FFA420", "RAL 2004": "#E55137", "RAL 2005": "#FF2301",
          "RAL 2007": "#FFA420", "RAL 2008": "#F44611", "RAL 2009": "#D84B20", "RAL 2010": "#EA5B0C",
          "RAL 2011": "#EC7C26", "RAL 2012": "#E55137", "RAL 2013": "#C35831", "RAL 3000": "#B32428",
          "RAL 3001": "#721422", "RAL 3002": "#A2231D", "RAL 3003": "#642424", "RAL 3004": "#4C1C24",
          "RAL 3005": "#5E2129", "RAL 3007": "#412227", "RAL 3009": "#642424", "RAL 3011": "#781F19",
          "RAL 3012": "#C1876B", "RAL 3013": "#A12312", "RAL 3014": "#D36E70", "RAL 3015": "#EA899A",
          "RAL 3016": "#B32821", "RAL 3017": "#E63244", "RAL 3018": "#D53032", "RAL 3020": "#CC0605",
          "RAL 3022": "#D95030", "RAL 3024": "#F80000", "RAL 3026": "#FE0000", "RAL 3027": "#B32428",
          "RAL 3028": "#E51A10", "RAL 3031": "#B32428", "RAL 3032": "#721422", "RAL 3033": "#A2231D",
          "RAL 4001": "#6D3F5B", "RAL 4002": "#922B3E", "RAL 4003": "#DE4C8A", "RAL 4004": "#641C34",
          "RAL 4005": "#6C4675", "RAL 4006": "#A03472", "RAL 4007": "#4A192C", "RAL 4008": "#924E7D",
          "RAL 4009": "#A18594", "RAL 4010": "#CF3476", "RAL 4011": "#8673A1", "RAL 4012": "#6C6874",
          "RAL 5000": "#354D73", "RAL 5001": "#1F3438", "RAL 5002": "#20214F", "RAL 5003": "#1D1E33",
          "RAL 5004": "#18171C", "RAL 5005": "#1E2460", "RAL 5007": "#3E5F8A", "RAL 5008": "#26252D",
          "RAL 5009": "#025669", "RAL 5010": "#0E294B", "RAL 5011": "#231A24", "RAL 5012": "#3B83BD",
          "RAL 5013": "#1E213D", "RAL 5014": "#606E8C", "RAL 5015": "#2271B3", "RAL 5017": "#063971",
          "RAL 5018": "#3F888F", "RAL 5019": "#1B5583", "RAL 5020": "#1D334A", "RAL 5021": "#256D7B",
          "RAL 5022": "#252850", "RAL 5023": "#49678D", "RAL 5024": "#5D9B9B", "RAL 5025": "#2A6478",
          "RAL 5026": "#102C54", "RAL 6000": "#316650", "RAL 6001": "#287233", "RAL 6002": "#2D572C",
          "RAL 6003": "#424632", "RAL 6004": "#1F3A3D", "RAL 6005": "#2F4538", "RAL 6006": "#2D3B24",
          "RAL 6007": "#343E40", "RAL 6008": "#3E4824", "RAL 6009": "#39352A", "RAL 6010": "#35682D",
          "RAL 6011": "#587246", "RAL 6012": "#343E40", "RAL 6013": "#6C7156", "RAL 6014": "#47402E",
          "RAL 6015": "#3B3C36", "RAL 6016": "#1E5945", "RAL 6017": "#4C9141", "RAL 6018": "#57A639",
          "RAL 6019": "#BDECB6", "RAL 6020": "#2E3A23", "RAL 6021": "#89AC76", "RAL 6022": "#25221B",
          "RAL 6024": "#308446", "RAL 6025": "#3D642D", "RAL 6026": "#015D52", "RAL 6027": "#84C3BE",
          "RAL 6028": "#2C5545", "RAL 6029": "#20603D", "RAL 6032": "#317F43", "RAL 6033": "#497E76",
          "RAL 6034": "#7FB5B5", "RAL 6035": "#1B4D3E", "RAL 6036": "#193737", "RAL 6037": "#00BB2D",
          "RAL 6038": "#00BB2D", "RAL 7000": "#78858B", "RAL 7001": "#8F999F", "RAL 7002": "#817F68",
          "RAL 7003": "#7E7461", "RAL 7004": "#9C9C9C", "RAL 7005": "#6C6960", "RAL 7006": "#6C635A",
          "RAL 7008": "#6E6243", "RAL 7009": "#4D5645", "RAL 7010": "#4C514A", "RAL 7011": "#434B4D",
          "RAL 7012": "#4E5754", "RAL 7013": "#464531", "RAL 7015": "#4C4F56", "RAL 7016": "#293133",
          "RAL 7021": "#23282B", "RAL 7022": "#332F2C", "RAL 7023": "#686C5E", "RAL 7024": "#474A51",
          "RAL 7026": "#1D262A", "RAL 7030": "#939388", "RAL 7031": "#5D6970", "RAL 7032": "#B9B9A8",
          "RAL 7033": "#7D8471", "RAL 7034": "#8F8B66", "RAL 7035": "#D7D7D7", "RAL 7036": "#7F7679",
          "RAL 7037": "#7D8471", "RAL 7038": "#B8B8B1", "RAL 7039": "#6C6960", "RAL 7040": "#9DA1AA",
          "RAL 7042": "#8D948D", "RAL 7043": "#4E5452", "RAL 7044": "#CAC4B0", "RAL 7045": "#909090",
          "RAL 7046": "#82898E", "RAL 7047": "#D0D0D0", "RAL 7048": "#898176", "RAL 8000": "#826C34",
          "RAL 8001": "#955F20", "RAL 8002": "#6C3B2A", "RAL 8003": "#734222", "RAL 8004": "#8E402A",
          "RAL 8007": "#59351F", "RAL 8008": "#6F4F28", "RAL 8011": "#5B3A29", "RAL 8012": "#592321",
          "RAL 8014": "#382C1E", "RAL 8015": "#633A34", "RAL 8016": "#4C2F27", "RAL 8017": "#45322E",
          "RAL 8019": "#403A3A", "RAL 8022": "#212121", "RAL 8023": "#A65E2E", "RAL 8024": "#79553D",
          "RAL 8025": "#755C48", "RAL 8028": "#4E3B31", "RAL 8029": "#763C28", "RAL 9001": "#FDF4E3",
          "RAL 9002": "#E7EBDA", "RAL 9003": "#F4F4F4", "RAL 9004": "#282828", "RAL 9005": "#0A0A0A",
          "RAL 9006": "#A5A5A5", "RAL 9007": "#8F8F8F", "RAL 9010": "#FFFFFF", "RAL 9011": "#1C1C1C",
          "RAL 9016": "#F6F6F6", "RAL 9017": "#1E1E1E", "RAL 9018": "#D7D7D7", "RAL 9022": "#9C9C9C",
          "RAL 9023": "#828282"
        };

        function getColorFromRAL(ral) {
          return RALColors[ral] || "#006400";
        }

        const optimizationSummary = [];
        const oversizedElements = [];
        const allContainers = [];
        let pageNumber = 1;

        function formatSheetElementSize(element) {
          return String(element.width) + " × " + String(element.height) + " мм";
        }

        function formatLinearElementSize(element) {
          if (isFinite(element.profileWidth)) {
            return String(element.length) + " мм (ширина профиля " + String(element.profileWidth) + " мм)";
          }

          return String(element.length) + " мм";
        }

        function pushOversizedSheetElement(element, sheetWidth, sheetHeight) {
          oversizedElements.push({
            label: element.label || "",
            material: element.material || "",
            color: element.color || "",
            type: "Лист",
            elementSize: formatSheetElementSize(element),
            stockSize: String(sheetWidth) + " × " + String(sheetHeight) + " мм",
            layer: element._layer || "",
            block: element._block || ""
          });
        }

        function pushOversizedLinearElement(element, stockLength) {
          oversizedElements.push({
            label: element.label || "",
            material: element.material || "",
            color: element.color || "",
            type: "Пруток",
            elementSize: formatLinearElementSize(element),
            stockSize: String(stockLength) + " мм",
            layer: element._layer || "",
            block: element._block || ""
          });
        }

        function createSVG() {
          const svgWrapper = document.createElement("div");
          svgWrapper.classList.add("svg-wrapper");

          const svgElement = document.createElementNS("http://www.w3.org/2000/svg", "svg");
          svgElement.setAttribute("width", "100%");
          svgElement.setAttribute("height", "100%");
          svgElement.setAttribute("viewBox", "0 0 794 1123");
          svgElement.setAttribute("xmlns", "http://www.w3.org/2000/svg");

          const footerTextElement = document.createElementNS("http://www.w3.org/2000/svg", "text");
          footerTextElement.setAttribute("x", 794 - 20);
          footerTextElement.setAttribute("y", 1123 - 20);
          footerTextElement.setAttribute("font-size", "16");
          footerTextElement.setAttribute("text-anchor", "end");
          footerTextElement.setAttribute("fill", "black");
          footerTextElement.textContent = String(pageNumber);

          svgElement.appendChild(footerTextElement);
          svgWrapper.appendChild(svgElement);
          svgContainer.appendChild(svgWrapper);

          pageNumber++;

          return {
            svg: svgElement,
            element: svgWrapper,
            footerTextElement: footerTextElement
          };
        }

        async function processGroup(data) {
          if (!data || !data.elements) {
            console.warn("⚠️ Пропущена пустая группа данных (нет поля elements).");
            return;
          }

          const requested = data.elements.reduce((sum,e)=>sum+(data.isLinear?e.length/1000:e.width*e.height/1000000),0);
          const viable=[];
          for(const element of data.elements){
            const fits=data.isLinear ? element.length<=data.sheetWidth : (element.width<=data.sheetWidth&&element.height<=data.sheetHeight)||(data.rotation===1&&element.height<=data.sheetWidth&&element.width<=data.sheetHeight);
            if(fits)viable.push(element);else if(data.isLinear)pushOversizedLinearElement(element,data.sheetWidth);else pushOversizedSheetElement(element,data.sheetWidth,data.sheetHeight);
          }
          const first=data.elements[0],before=optimizationSummary.length;
          if(viable.length){const prepared={...data,elements:viable};if(data.isLinear)await processLinearGroup(prepared);else await processSheetGroup(prepared);}
          if(optimizationSummary.length===before)optimizationSummary.push({material:first.material,color:first.color,isLinear:data.isLinear,sheetWidth:data.sheetWidth,sheetHeight:data.sheetHeight,stockLength:data.sheetWidth,usedBars:0,usedContainers:0,totalElementLength:0,totalElementArea:0,totalStockLength:0,totalContainerArea:0,totalElementsCount:0,materialWaste:0});
          optimizationSummary[optimizationSummary.length-1].requestedQty=CuttingData.round(requested);
        }

        async function processSheetGroup(data) {
          const sheetWidth = parseDimension(data.sheetWidth);
          const sheetHeight = parseDimension(data.sheetHeight);
          const allowRotation = data.rotation;
          const sawWidth = isFinite(data.sawWidth) ? data.sawWidth : 0;
          const elements = data.elements;

          if (!isFinite(sheetWidth) || sheetWidth <= 0 || !isFinite(sheetHeight) || sheetHeight <= 0) {
            console.error("❌ Неверный размер листа: L=" + data.sheetWidth + " H=" + data.sheetHeight + ". Группа будет пропущена.");
            return;
          }

          if (!elements.length) {
            console.warn("⚠️ Пустая листовая группа (0 элементов). Пропуск.");
            return;
          }

          const elementSpacing = 30;
          let totalElementArea = 0;
          let totalContainerArea = 0;
          let totalElementsCount = 0;
          let currentSVG = null;
          let currentContainerY = 0;
          let currentContainerX = 0;
          const containers = [];
          let containerId = 0;

          elements.sort(function(a, b) {
            const aw = parseDimension(a.width);
            const ah = parseDimension(a.height);
            const bw = parseDimension(b.width);
            const bh = parseDimension(b.height);
            return (bw * bh) - (aw * ah);
          });

          function createContainer(material, color) {
            const id = containerId++;
            const scaleX = (794 - 2 * elementSpacing) / sheetWidth;
            const scaleY = (1123 - 2 * elementSpacing - 80) / sheetHeight;
            const scale = Math.min(scaleX, scaleY);

            if (!isFinite(scale) || scale <= 0) {
              console.error("❌ Не удалось вычислить масштаб для листа " + sheetWidth + "x" + sheetHeight + ". Контейнер пропущен.");
              return null;
            }

            const scaledWidth = sheetWidth * scale;
            const scaledHeight = sheetHeight * scale;

            if (!currentSVG || currentContainerY + scaledHeight + elementSpacing > 1123 - elementSpacing) {
              currentSVG = createSVG();
              currentContainerY = 80;
              currentContainerX = elementSpacing;
            } else if (currentContainerX + scaledWidth + elementSpacing > 794 - elementSpacing) {
              currentContainerX = elementSpacing;
              currentContainerY += scaledHeight + elementSpacing;

              if (currentContainerY + scaledHeight + elementSpacing > 1123 - elementSpacing) {
                currentSVG = createSVG();
                currentContainerY = 80;
                currentContainerX = elementSpacing;
              }
            }

            totalContainerArea += sheetWidth * sheetHeight;

            const container = {
              id: id,
              svg: currentSVG.svg,
              element: currentSVG.element,
              x: currentContainerX,
              y: currentContainerY,
              width: sheetWidth,
              height: sheetHeight,
              scale: scale,
              elements: [],
              material: material,
              color: color,
              freeRectangles: [{ x: 0, y: 0, width: sheetWidth, height: sheetHeight }]
            };

            currentContainerX += scaledWidth + elementSpacing;
            drawContainerBorders(container);
            drawContainerHeader(container, material, color, sheetWidth, sheetHeight);
            allContainers.push(container);

            return container;
          }

          function addElementToContainer(element) {
            for (let c = 0; c < containers.length; c++) {
              const container = containers[c];

              if (container.material === element.material && container.color === element.color) {
                const position = findBestFitPosition(container, element);

                if (position) {
                  placeElement(container, element, position);
                  totalElementArea += element.width * element.height;
                  totalElementsCount++;
                  return;
                }
              }
            }

            const newContainer = createContainer(elements[0].material, elements[0].color);

            if (!newContainer) {
              console.error("❌ Не удалось создать контейнер для листовой группы. Элемент будет пропущен:", element);
              pushOversizedSheetElement(element, sheetWidth, sheetHeight);
              return;
            }

            containers.push(newContainer);

            const position = findBestFitPosition(newContainer, element);

            if (position) {
              placeElement(newContainer, element, position);
              totalElementArea += element.width * element.height;
              totalElementsCount++;
            } else {
              pushOversizedSheetElement(element, sheetWidth, sheetHeight);
            }
          }

          function findBestFitPosition(container, element) {
            let bestFit = null;
            let minWaste = Infinity;

            for (let r = 0; r < container.freeRectangles.length; r++) {
              const rect = container.freeRectangles[r];

              if (rect.width >= element.width && rect.height >= element.height) {
                const waste = (rect.width - element.width) * (rect.height - element.height);

                if (waste < minWaste) {
                  minWaste = waste;
                  bestFit = { x: rect.x, y: rect.y, width: element.width, height: element.height, rotated: false };
                }
              }

              if (allowRotation === 1 && rect.width >= element.height && rect.height >= element.width) {
                const wasteRot = (rect.width - element.height) * (rect.height - element.width);

                if (wasteRot < minWaste) {
                  minWaste = wasteRot;
                  bestFit = { x: rect.x, y: rect.y, width: element.height, height: element.width, rotated: true };
                }
              }
            }

            return bestFit;
          }

          function placeElement(container, element, position) {
            if (position.rotated) {
              const tmpW = element.width;
              element.width = element.height;
              element.height = tmpW;
            }

            element.x = position.x;
            element.y = position.y;
            container.elements.push(element);
            splitFreeRectangles(container, position);
            drawElement(container, element);
          }

          function splitFreeRectangles(container, placedElement) {
            const newRectangles = [];

            for (let i = 0; i < container.freeRectangles.length; i++) {
              const rect = container.freeRectangles[i];

              if (intersects(rect, placedElement)) {
                if (rect.x < placedElement.x) {
                  newRectangles.push({
                    x: rect.x,
                    y: rect.y,
                    width: placedElement.x - rect.x - sawWidth,
                    height: rect.height
                  });
                }

                if (rect.x + rect.width > placedElement.x + placedElement.width) {
                  newRectangles.push({
                    x: placedElement.x + placedElement.width + sawWidth,
                    y: rect.y,
                    width: rect.x + rect.width - (placedElement.x + placedElement.width + sawWidth),
                    height: rect.height
                  });
                }

                if (rect.y < placedElement.y) {
                  newRectangles.push({
                    x: rect.x,
                    y: rect.y,
                    width: rect.width,
                    height: placedElement.y - rect.y - sawWidth
                  });
                }

                if (rect.y + rect.height > placedElement.y + placedElement.height) {
                  newRectangles.push({
                    x: rect.x,
                    y: placedElement.y + placedElement.height + sawWidth,
                    width: rect.width,
                    height: rect.y + rect.height - (placedElement.y + placedElement.height + sawWidth)
                  });
                }
              } else {
                newRectangles.push(rect);
              }
            }

            container.freeRectangles = newRectangles;
          }

          function intersects(rect1, rect2) {
            return !(
              rect2.x >= rect1.x + rect1.width ||
              rect2.x + rect2.width <= rect1.x ||
              rect2.y >= rect1.y + rect1.height ||
              rect2.y + rect2.height <= rect1.y
            );
          }

          function drawElement(container, element) {
            if (!isFinite(element.width) || !isFinite(element.height)) {
              console.error("❌ Невалидные габариты элемента при отрисовке (width/height NaN). Пропуск.", element);
              return;
            }

            const fillColor = getColorFromRAL(element.color);
            const scale = container.scale;

            const rect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
            rect.setAttribute("x", String(container.x + element.x * scale));
            rect.setAttribute("y", String(container.y + element.y * scale));
            rect.setAttribute("width", String(element.width * scale));
            rect.setAttribute("height", String(element.height * scale));
            rect.setAttribute("fill", fillColor);
            rect.setAttribute("stroke", "black");
            container.svg.appendChild(rect);

            const textSize = Math.min(element.width * scale / 7, element.height * scale / 7);

            const textLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
            textLabel.setAttribute("x", String(container.x + (element.x + element.width / 2) * scale));
            textLabel.setAttribute("y", String(container.y + (element.y + element.height / 3) * scale));
            textLabel.setAttribute("font-size", String(textSize));
            textLabel.setAttribute("text-anchor", "middle");
            textLabel.setAttribute("fill", "black");
            textLabel.textContent = element.label;
            container.svg.appendChild(textLabel);

            const textSizeDetails = document.createElementNS("http://www.w3.org/2000/svg", "text");
            textSizeDetails.setAttribute("x", String(container.x + (element.x + element.width / 2) * scale));
            textSizeDetails.setAttribute("y", String(container.y + (element.y + 2 * element.height / 3) * scale));
            textSizeDetails.setAttribute("font-size", String(textSize));
            textSizeDetails.setAttribute("text-anchor", "middle");
            textSizeDetails.setAttribute("fill", "black");
            textSizeDetails.textContent = String(element.width) + "x" + String(element.height);
            container.svg.appendChild(textSizeDetails);
          }

          function drawContainerBorders(container) {
            const scale = container.scale;
            const w = container.width * scale;
            const h = container.height * scale;

            if (!isFinite(w) || !isFinite(h)) {
              console.error("❌ Невалидные размеры контейнера при отрисовке рамки (NaN). Пропуск рамки.", container);
              return;
            }

            const rect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
            rect.setAttribute("x", String(container.x));
            rect.setAttribute("y", String(container.y));
            rect.setAttribute("width", String(w));
            rect.setAttribute("height", String(h));
            rect.setAttribute("stroke", "black");
            rect.setAttribute("stroke-width", "2");
            rect.setAttribute("fill", "none");
            container.svg.appendChild(rect);
          }

          function drawContainerHeader(container, material, color, width, height) {
            const textHeader = document.createElementNS("http://www.w3.org/2000/svg", "text");
            textHeader.setAttribute("x", String(container.x + (container.width * container.scale) / 2));
            textHeader.setAttribute("y", String(container.y - 10));
            textHeader.setAttribute("font-size", "14");
            textHeader.setAttribute("text-anchor", "middle");
            textHeader.setAttribute("fill", "black");

            let headerText = "Материал: " + material + ", Размер: " + width + "x" + height;

            if (color && String(color).trim() !== "") {
              headerText += ", Цвет: " + color;
            }

            textHeader.textContent = headerText;
            container.svg.appendChild(textHeader);
          }

          function calculateMaterialUsage() {
            const usagePercent = totalContainerArea > 0 ? (totalElementArea / totalContainerArea) * 100 : 0;
            const usedContainers = allContainers.filter(function(c) {
              return c.material === elements[0].material && c.color === elements[0].color;
            }).length;

            optimizationSummary.push({
              material: elements[0].material,
              color: elements[0].color,
              sheetWidth: sheetWidth,
              sheetHeight: sheetHeight,
              usedContainers: usedContainers,
              totalElementsCount: totalElementsCount,
              materialWaste: (100 - usagePercent).toFixed(2),
              totalContainerArea: CuttingData.round(totalContainerArea / 1000000),
              totalElementArea: CuttingData.round(totalElementArea / 1000000),
              isLinear: false
            });
          }

          const prepared = [];

          for (let i = 0; i < elements.length; i++) {
            const el = Object.assign({}, elements[i]);
            el.width = parseDimension(el.width);
            el.height = parseDimension(el.height);

            if (!isFinite(el.width) || !isFinite(el.height) || el.width <= 0 || el.height <= 0) {
              console.error("❌ Невалидный элемент (листовой): \"" + (el._raw || "") + "\" (layer: " + (el._layer || "") + ", block: " + (el._block || "") + "). Пропуск.");
              continue;
            }

            prepared.push(el);
          }

          if (!prepared.length) {
            console.warn("⚠️ В листовой группе после валидации не осталось элементов. Пропуск.");
            return;
          }

          const firstContainer = createContainer(prepared[0].material, prepared[0].color);

          if (!firstContainer) {
            console.error("❌ Не удалось создать первый контейнер для листовой группы. Пропуск всей группы.");

            for (let i = 0; i < prepared.length; i++) {
              pushOversizedSheetElement(prepared[i], sheetWidth, sheetHeight);
            }

            return;
          }

          containers.push(firstContainer);

          for (let i = 0; i < prepared.length; i++) {
            const element = prepared[i];
            addElementToContainer(element);
            await new Promise(function(resolve) {
              setTimeout(resolve, 0);
            });
          }

          calculateMaterialUsage();
          console.log("Все контейнеры (листовые):", containers);
        }

        async function processLinearGroup(data) {
          if (!data.elements || !data.elements.length) {
            console.warn("⚠️ Пустая линейная группа (0 элементов). Пропуск.");
            return;
          }

          const stockLength = parseDimension(data.sheetWidth);
          const sawWidth = isFinite(data.sawWidth) ? data.sawWidth : 0;

          if (!isFinite(stockLength) || stockLength <= 0) {
            console.error("❌ Неверная длина прутка (L палки): \"" + data.sheetWidth + "\". Группа будет пропущена.");
            return;
          }

          const elements = data.elements;
          const elementSpacing = 30;
          let totalElementLength = 0;
          let totalStockUsed = 0;
          let totalElementsCount = 0;
          let currentSVG = null;
          let currentContainerY = 0;
          const containers = [];
          let containerId = 0;

          const prepared = [];

          for (let i = 0; i < elements.length; i++) {
            const e = Object.assign({}, elements[i]);
            e.length = parseDimension(e.length);

            if (!isFinite(e.length) || e.length <= 0) {
              console.error("❌ Невалидный линейный элемент: \"" + (e._raw || "") + "\" (layer: " + (e._layer || "") + ", block: " + (e._block || "") + "). Пропуск.");
              continue;
            }

            prepared.push(e);
          }

          if (!prepared.length) {
            console.warn("⚠️ В линейной группе после валидации не осталось элементов. Пропуск.");
            return;
          }

          prepared.sort(function(a, b) {
            return b.length - a.length;
          });

          function createContainer(material, color) {
            const id = containerId++;
            const scale = (794 - 2 * elementSpacing) / stockLength;

            if (!isFinite(scale) || scale <= 0) {
              console.error("❌ Не удалось вычислить масштаб для прутка длиной " + stockLength + ". Контейнер пропущен.");
              return null;
            }

            const barHeight = 50;

            if (!currentSVG || currentContainerY + barHeight + elementSpacing > 1123 - elementSpacing) {
              currentSVG = createSVG();
              currentContainerY = 80;
            }

            totalStockUsed += stockLength;

            const container = {
              id: id,
              svg: currentSVG.svg,
              element: currentSVG.element,
              x: elementSpacing,
              y: currentContainerY,
              length: stockLength,
              scale: scale,
              elements: [],
              material: material,
              color: color,
              freeSegments: [{ x: 0, length: stockLength }]
            };

            currentContainerY += barHeight + elementSpacing;
            drawBarBorders(container, barHeight);
            drawBarHeader(container, material, color, stockLength);
            allContainers.push(container);

            return container;
          }

          function addElementToContainer(element) {
            for (let i = 0; i < containers.length; i++) {
              const container = containers[i];

              if (container.material === element.material && container.color === element.color) {
                const position = findBestFitPosition(container, element);

                if (position !== null) {
                  placeElement(container, element, position);
                  totalElementLength += element.length;
                  totalElementsCount++;
                  return;
                }
              }
            }

            const newContainer = createContainer(prepared[0].material, prepared[0].color);

            if (!newContainer) {
              console.error("❌ Не удалось создать контейнер для линейной группы. Элемент будет пропущен:", element);
              pushOversizedLinearElement(element, stockLength);
              return;
            }

            containers.push(newContainer);

            const position = findBestFitPosition(newContainer, element);

            if (position !== null) {
              placeElement(newContainer, element, position);
              totalElementLength += element.length;
              totalElementsCount++;
            } else {
              pushOversizedLinearElement(element, stockLength);
            }
          }

          function findBestFitPosition(container, element) {
            for (let i = 0; i < container.freeSegments.length; i++) {
              const segment = container.freeSegments[i];

              if (segment.length >= element.length) {
                return segment.x;
              }
            }

            return null;
          }

          function placeElement(container, element, positionX) {
            element.x = positionX;
            container.elements.push(element);
            splitFreeSegments(container, element, positionX);
            drawLinearElement(container, element);
          }

          function splitFreeSegments(container, placedElement, positionX) {
            const newSegments = [];

            for (let i = 0; i < container.freeSegments.length; i++) {
              const segment = container.freeSegments[i];

              if (positionX >= segment.x && positionX < segment.x + segment.length) {
                if (positionX > segment.x) {
                  newSegments.push({
                    x: segment.x,
                    length: positionX - segment.x - sawWidth
                  });
                }

                if (positionX + placedElement.length + sawWidth < segment.x + segment.length) {
                  newSegments.push({
                    x: positionX + placedElement.length + sawWidth,
                    length: segment.x + segment.length - (positionX + placedElement.length + sawWidth)
                  });
                }
              } else {
                newSegments.push(segment);
              }
            }

            container.freeSegments = newSegments;
          }

          function drawLinearElement(container, element) {
            if (!isFinite(element.length) || element.length <= 0) {
              console.error("❌ Невалидная длина линейного элемента при отрисовке (NaN). Пропуск.", element);
              return;
            }

            const fillColor = getColorFromRAL(element.color);
            const scale = container.scale;
            const barHeight = 50;

            const rect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
            rect.setAttribute("x", String(container.x + element.x * scale));
            rect.setAttribute("y", String(container.y));
            rect.setAttribute("width", String(element.length * scale));
            rect.setAttribute("height", String(barHeight));
            rect.setAttribute("fill", fillColor);
            rect.setAttribute("stroke", "black");
            container.svg.appendChild(rect);

            const textSize = barHeight / 3;

            const textLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
            textLabel.setAttribute("x", String(container.x + (element.x + element.length / 2) * scale));
            textLabel.setAttribute("y", String(container.y + barHeight / 2 - textSize / 2));
            textLabel.setAttribute("font-size", String(textSize));
            textLabel.setAttribute("text-anchor", "middle");
            textLabel.setAttribute("fill", "black");
            textLabel.textContent = element.label;
            container.svg.appendChild(textLabel);

            const textLength = document.createElementNS("http://www.w3.org/2000/svg", "text");
            textLength.setAttribute("x", String(container.x + (element.x + element.length / 2) * scale));
            textLength.setAttribute("y", String(container.y + barHeight / 2 + textSize));
            textLength.setAttribute("font-size", String(textSize));
            textLength.setAttribute("text-anchor", "middle");
            textLength.setAttribute("fill", "black");
            textLength.textContent = String(element.length);
            container.svg.appendChild(textLength);
          }

          function drawBarBorders(container, barHeight) {
            const scale = container.scale;
            const width = container.length * scale;

            if (!isFinite(width)) {
              console.error("❌ Невалидная ширина прямоугольника (width NaN) при отрисовке прутка. Пропуск рамки.", container);
              return;
            }

            const rect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
            rect.setAttribute("x", String(container.x));
            rect.setAttribute("y", String(container.y));
            rect.setAttribute("width", String(width));
            rect.setAttribute("height", String(barHeight));
            rect.setAttribute("stroke", "black");
            rect.setAttribute("stroke-width", "2");
            rect.setAttribute("fill", "none");
            container.svg.appendChild(rect);
          }

          function drawBarHeader(container, material, color, length) {
            const textHeader = document.createElementNS("http://www.w3.org/2000/svg", "text");
            textHeader.setAttribute("x", String(container.x + (container.length * container.scale) / 2));
            textHeader.setAttribute("y", String(container.y - 10));
            textHeader.setAttribute("font-size", "14");
            textHeader.setAttribute("text-anchor", "middle");
            textHeader.setAttribute("fill", "black");

            let headerText = "Материал: " + material + ", Длина прутка: " + length;

            if (color && String(color).trim() !== "") {
              headerText += ", Цвет: " + color;
            }

            textHeader.textContent = headerText;
            container.svg.appendChild(textHeader);
          }

          function calculateMaterialUsage() {
            const usagePercent = totalStockUsed > 0 ? (totalElementLength / totalStockUsed) * 100 : 0;
            const usedContainers = allContainers.filter(function(c) {
              return c.material === prepared[0].material && c.color === prepared[0].color;
            }).length;

            optimizationSummary.push({
              material: prepared[0].material,
              color: prepared[0].color,
              stockLength: stockLength,
              usedBars: usedContainers,
              totalElementsCount: totalElementsCount,
              materialWaste: (100 - usagePercent).toFixed(2),
              totalStockLength: CuttingData.round(totalStockUsed / 1000),
              totalElementLength: CuttingData.round(totalElementLength / 1000),
              isLinear: true
            });
          }

          const firstContainer = createContainer(prepared[0].material, prepared[0].color);

          if (!firstContainer) {
            console.error("❌ Не удалось создать первый контейнер для линейной группы. Пропуск всей группы.");

            for (let i = 0; i < prepared.length; i++) {
              pushOversizedLinearElement(prepared[i], stockLength);
            }

            return;
          }

          containers.push(firstContainer);

          for (let i = 0; i < prepared.length; i++) {
            const element = prepared[i];
            addElementToContainer(element);
            await new Promise(function(resolve) {
              setTimeout(resolve, 0);
            });
          }

          calculateMaterialUsage();
          console.log("Все линейные контейнеры:", containers);
        }

        let reportSVGs = [];
        let currentSVGObj = null;
        let currentSVG = null;
        let reportY = 80;

        function createReportSVG() {
          currentSVGObj = createSVG();
          currentSVG = currentSVGObj.svg;
          reportY = 80;

          const reportTitle = document.createElementNS("http://www.w3.org/2000/svg", "text");
          reportTitle.setAttribute("x", String(794 / 2));
          reportTitle.setAttribute("y", "40");
          reportTitle.setAttribute("font-size", "24");
          reportTitle.setAttribute("text-anchor", "middle");
          reportTitle.setAttribute("fill", "black");
          reportTitle.textContent = "Рекомендация по раскрою материала";

          currentSVG.appendChild(reportTitle);
          reportSVGs.push(currentSVGObj);
        }

        function addReportText(text) {
          if (!currentSVG) {
            createReportSVG();
          }

          const lines = String(text).split("\n");

          lines.forEach(function(line) {
            if (reportY + 20 > 1123 - 100) {
              createReportSVG();
            }

            const reportLine = document.createElementNS("http://www.w3.org/2000/svg", "text");
            reportLine.setAttribute("x", "20");
            reportLine.setAttribute("y", String(reportY));
            reportLine.setAttribute("font-size", "16");
            reportLine.setAttribute("text-anchor", "start");
            reportLine.setAttribute("fill", "black");
            reportLine.textContent = line;
            currentSVG.appendChild(reportLine);
            reportY += 20;
          });

          reportY += 40;
        }

        function sendJsonData() {
          const tableRows = optimizationSummary.map(summary => {
            const size=summary.isLinear?summary.stockLength/1000:summary.sheetWidth*summary.sheetHeight/1000000;
            const count=summary.isLinear?summary.usedBars:summary.usedContainers;
            return {material:summary.material,name:summary.material,label:summary.isLinear?String(summary.stockLength)+' мм':String(summary.sheetWidth)+'×'+String(summary.sheetHeight)+' мм',color:summary.color,group:summary.isLinear?'Подоблицовочный металлокаркас':'Облицовочные материалы',unit:summary.isLinear?'м.пог':'м²',projectQty:summary.requestedQty,size,cutQty:CuttingData.round(count*size),cutMap:count+' '+(summary.isLinear?'прутков':'листов')+'; отходы '+summary.materialWaste+'%'};
          });

          optimizationSummary.forEach(function(summary) {
            let reportText = "";

            if (summary.isLinear) {
              reportText += "Материал: " + summary.material;

              if (summary.color && String(summary.color).trim() !== "") {
                reportText += ", Цвет: " + summary.color;
              }

              reportText += "\n";
              reportText += "Длина прутка: " + summary.stockLength + " мм\n";
              reportText += "Использовано прутков: " + summary.usedBars + "\n";
              reportText += "Общее количество элементов: " + summary.totalElementsCount + "\n";
              reportText += "Общий расход материала: " + summary.totalStockLength + " м\n";
              reportText += "Общая длина элементов: " + summary.totalElementLength + " м\n";
              reportText += "Отходы материала: " + summary.materialWaste + "%\n";
            } else {
              reportText += "Материал: " + summary.material;

              if (summary.color && String(summary.color).trim() !== "") {
                reportText += ", Цвет: " + summary.color;
              }

              reportText += "\n";
              reportText += "Размер листа: " + summary.sheetWidth + "x" + summary.sheetHeight + " мм\n";
              reportText += "Использовано листов: " + summary.usedContainers + "\n";
              reportText += "Общее количество элементов: " + summary.totalElementsCount + "\n";
              reportText += "Общий расход материала: " + summary.totalContainerArea + " м²\n";
              reportText += "Общая площадь элементов: " + summary.totalElementArea + " м²\n";
              reportText += "Отходы материала: " + summary.materialWaste + "%\n";
            }

            addReportText(reportText);
          });

          const footerText = "Карта раскроя учитывает заданные припуски и ширину пропила.\nЗапас материала задаётся отдельно в спецификации. Неразмещённые детали указаны в таблице.";
          addReportText(footerText);

          reportSVGs.reverse().forEach(function(svgObj) {
            svgContainer.prepend(svgObj.element);
          });

          const allSVGs = svgContainer.querySelectorAll("svg");

          for (let i = 0; i < allSVGs.length; i++) {
            const svg = allSVGs[i];
            const footerTextElement = svg.querySelector("text");

            if (footerTextElement) {
              footerTextElement.textContent = String(i + 1);
            }
          }

          renderOversizedTable(oversizedElements);

          CuttingTable.setCutRows(tableRows);
        }

        async function processDataGroups() {
          jsonProgressContainer.classList.add("visible");
          jsonProgressBar.style.width = "5%";
          jsonProgressBar.innerText = "5%";

          await new Promise(function(resolve) {
            setTimeout(resolve, 150);
          });

          const totalGroups = dataGroups.length;

          for (let i = 0; i < totalGroups; i++) {
            const group = dataGroups[i];
            await processGroup(group);

            const progress = Math.round(5 + ((i + 1) / totalGroups) * 95);
            jsonProgressBar.style.width = String(progress) + "%";
            jsonProgressBar.innerText = String(progress) + "%";

            await new Promise(function(resolve) {
              setTimeout(resolve, 0);
            });
          }

          sendJsonData();
          downloadPdfButton.disabled = false;
          jsonProgressContainer.classList.remove("visible");
        }

        await processDataGroups();
      }

      downloadPdfButton.onclick = function(event) {
        event.preventDefault();event.stopPropagation();
        if(downloadPdfButton.disabled)return;
        document.body.dataset.printMode='map';window.print();
      };
    })();
  