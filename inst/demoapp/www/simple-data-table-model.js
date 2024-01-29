
class SimpleModelDataTable {
    constructor($el, options = {}) {
        this.$el = $el;
        if (options.withAddButton == false) this.withAddButton = false;
        else this.withAddButton = true;
        this.addButtonLabel = options.addButtonLabel || '✚';
        if (options.withRemoveButton == false) this.withRemoveButton = false;
        else this.withRemoveButton = true;
        this.keyColumnReadonly = [1, 3, 5];
        this.defaultColumnPrefix = options.defaultColumnPrefix || 'column';
        this.defaultColumnNumber = options.defaultColumnNumber || null;
        this.defaultHighlightedCellClass = options.defaultHighlightedCellClass || 'highlighted-cell';
        this.headers = [];
        this.data = [];
        this._events = {};
        this.colWidth = options.colWidth;
        this.maxRowCount = options.maxRowCount || 3;
        if (options.target == 'con_mcpmod') this.modelTypeArray = conModelTypeArray;
        else if (options.target == 'bin_mcpmod') this.modelTypeArray = binModelTypeArray;
    }

    _renderTHead($table) {
        const $thead = document.createElement('thead');
        const $row = document.createElement('tr');
        this.headers.forEach((name, index) => {
            if (index <= 2) {
                let $cell;
                if (index < 2) $cell = this._createEmptyHeaderCell(this.colWidth[index]);
                else $cell = this._createEmptyHeaderCell(this.colWidth[index] * 4, 4);
                if (name == 'ID') $cell.textContent = 'No.';
                else $cell.textContent = name;
                $row.appendChild($cell);
            }
        });
        const $cell = this._createEmptyHeaderCell(2);
        $cell.textContent = '';
        $cell.style['background']='none';
        $cell.style['border']='none';
        $row.appendChild($cell);

        $thead.appendChild($row);
        $table.appendChild($thead);
    }

    //render时执行,table的数据部分
    _renderTBody($table) {
        const $tbody = document.createElement('tbody');

        this.data.forEach((item, rowIndex) => {
            const $row = document.createElement('tr');
            
            Object.entries(item).forEach(([key, value], colIndex) => {
                //console.log('_renderTBody: ');
                //console.log('_renderTBody - row: ' + rowIndex);
                //console.log('name: ' + key + ', value: ' + value + ', row: '+ rowIndex + ', col: ' + colIndex);
                if (key == this.defaultColumnPrefix + 1) value = rowIndex + 1;
                const $cell = this._createCellWithInput(key, value, rowIndex, colIndex + 1);
                $row.appendChild($cell);
            });

            const $cell = this._createCell(rowIndex);
            $row.appendChild($cell);
            $tbody.appendChild($row);
        });

        $table.appendChild($tbody);
    }

    render() {
        if (!this.$el) {
            throw new Error('this.$el is not defined');
        }

        SimpleDataTable.clearElement(this.$el);

        const $wrapper = document.createElement('div');
        $wrapper.classList.add('simple-model-data-table');
        const $table = document.createElement('table');
        $table.className = "simple-model-data-table";
        
        if (this.headers.length > 0) {
            this._renderTHead($table);
        }

        this._renderTBody($table);

        $wrapper.appendChild($table);

//console.log('data.length' + this.data.length);
//console.log('maxrowcount' + this.maxRowCount);
        // 判断是否需要添加add button
        if (this.withAddButton) {
            if (this.data.length < this.maxRowCount) {
            const $addButton = this._createAddButton();
            $wrapper.appendChild($addButton);
            }
        }

        this.$el.appendChild($wrapper);
        return this;
    }

    findCellsByContent(...content) {
        const indexes = [];
        const $rows = this.$el.querySelectorAll('tr');

        $rows.forEach((row, rowIndex) => {
            const cells = row.querySelectorAll('td');

            cells.forEach((cell, cellIndex) => {
                const $cellInput = cell.querySelector('input');
                const cellContent = $cellInput
                    ? $cellInput.value
                    : cell.textContent;

                content.forEach((item) => {
                    if (cellContent === item) {
                        indexes.push({
                            rowIndex,
                            cellIndex,
                        });
                    }
                });
            });
        });
        return indexes;
    }

    getCell(rowIndex, cellIndex) {
        const $rows = this.$el.querySelectorAll('tr');
        const $row = $rows[rowIndex];

        if (!$row) {
            return null;
        }

        const $cells = $row.querySelectorAll('td');
        const $cell = $cells[cellIndex];

        if (!$cell) {
            return null;
        }

        return $cell;
    }

    highlightCell(rowIndex, cellIndex) {
        const $cell = this.getCell(rowIndex, cellIndex);
        $cell.classList.add(this.defaultHighlightedCellClass);
    }

    clearHighlightedCells() {
        const $cells = this.$el.querySelectorAll('td');
        $cells.forEach(($cell) => {
            $cell.classList.remove(this.defaultHighlightedCellClass);
        });
    }

    setInputCellContent(rowIndex, cellIndex, content) {
        const $cell = this.getCell(rowIndex, cellIndex);
        const $input = $cell.querySelector('input');
        $input.value = content;
    }

    _createEmptyCell() {
        return document.createElement('td');
    }

    _createEmptyHeaderCell(width, colspan) {
        const $cell = document.createElement('th');
        $cell.style.width = width + "%";
        //合并单元格
        if (colspan != undefined) $cell.setAttribute('colspan', colspan);
        return $cell;
    }

    // 创建带/不带remove button的空单元格
    _createCell(rowId) {
        const $cell = this._createEmptyCell();
        //if (rowId > 0 && this.withRemoveButton) {
        if (this.withRemoveButton) {
            const $removeButton = document.createElement('button');
            $removeButton.classList.add('remove-row');
            $removeButton.textContent = '✖︎';
            $removeButton.addEventListener('click', () => {
                const $tr = $cell.parentNode;
                this._removeRow($tr);
            });
            $cell.appendChild($removeButton);
        }
        return $cell;
    }

    _removeRow($tr) {
        const $siblings = Array.from($tr.parentNode.children);
        const index = $siblings.indexOf($tr);
        this.data.splice(index, 1);
        
        //console.log('_removeRow');
        this.data.forEach((item, rowIndex) => {
            //console.log(rowIndex);
            //console.log(item);
            if (rowIndex >= index) this.data[rowIndex][this.defaultColumnPrefix + 1] = rowIndex + 1;
        });
        
        $tr.remove();
        this.emit(SimpleDataTable.EVENTS.ROW_REMOVED, index);
        this.emit(SimpleDataTable.EVENTS.UPDATE, this.data);
        this.render();
    }

    _createAddButton() {
        const $addButton = document.createElement('button');
        $addButton.classList.add('add-row');
        $addButton.textContent = this.addButtonLabel;
        $addButton.addEventListener('click', this._createEmptyRow.bind(this));
        return $addButton;
    }
    
    // colIndex从1开始
    _createCellWithInput(name, value, rowIndex, colIndex) {
        //console.log("_createCellWithInput:");
        //console.log('name：' + name + ', value: ' + value + ', rowIndex: ' + rowIndex + ', colIndex: ' + colIndex);
        const $cell = document.createElement('td');
        $cell.style.width = this.colWidth[colIndex - 1] + "%";
        if (this.keyColumnReadonly.indexOf(colIndex) > -1) {
            // b控件
            //console.log(name + ': b' + ', value: ' + value);
            const $div = document.createElement('div');
            const $b = document.createElement('b');
            if (value == null || value == NaN || value == '') {
                $cell.style['background-color']='#fafafa';
            }
            $b.innerHTML = value;
            $div.appendChild($b);
            $cell.appendChild($div);
        } else if (colIndex == 2) {
            // select控件：Model type
            //console.log(name + ': select' + ', value: ' + value);
            let modelTypeArray = new Array();
            const $div = document.createElement('div');
            const $select = document.createElement('select');
            $select.name = name;
            const $placeholder = document.createElement('option');
            $placeholder.innerText = 'Select model type';
            $placeholder.setAttribute('disabled','disabled');
            //console.log('+++' + value + '+++');
            if (value === '') $placeholder.setAttribute('selected','selected');
            $select.appendChild($placeholder);
            // (this.target == 'con_mcpmod') modelTypeArray = conModelTypeArray;
            //else if (this.target == 'bin_mcpmod') modelTypeArray = binModelTypeArr;
            this.modelTypeArray.forEach(item => {
                const $option = document.createElement('option');
                $option.setAttribute('value',item[0]);
                if (item[1] == 0) {
                    $option.setAttribute('disabled','disabled');
                } 
                $option.innerText = item[0];
                if (item[0] == value) $option.setAttribute('selected','selected');

                $select.appendChild($option);
                
            });
            $div.appendChild($select);
            $cell.appendChild($div);
            $select.addEventListener('change', () => {
                this.data[rowIndex][name] = $select.value;
                //更新model下拉列表的可选项
                let linear_active = 1, loglinear_active = 1;
                this.data.forEach((item) => {
                    if (item.column2 == 'Linear') linear_active = 0;
                    else if (item.column2 == 'Log-Linear') loglinear_active = 0;
                });
                this.modelTypeArray[0][1] = linear_active;
                this.modelTypeArray[3][1] = loglinear_active;
                //刷新table
                this.emit(SimpleDataTable.EVENTS.UPDATE, this.data);
            });
        } else {
            // input控件
            //console.log(name + ': input' + ', value: ' + value);
            const $input = document.createElement('input');
            $input.value = value;
            $input.name = name;
            if (value === null) {
                $input.disabled = true;
            }
            $cell.appendChild($input);
            $input.addEventListener('change', () => {
                this.data[rowIndex][name] = $input.value;
                this.emit(SimpleDataTable.EVENTS.UPDATE, this.data);
            });
        }
        return $cell;
    }

    //点+按钮时执行
    _createEmptyRow() {
        const $tbody = this.$el.querySelector('tbody');
        const rowsCount = $tbody.querySelectorAll('tr').length;
        const $row = document.createElement('tr');
        const columnNames = this._fetchColumnNames();
        //console.log(columnNames);
        const record = {};
        // colId从1开始
        columnNames.forEach((cellName, colId) => {
            //console.log('_createEmptyRow:');
            //console.log('cellName: ' + cellName + ', colId: ' + colId);
            let $cell;
            if (colId == 1) {
                $cell = this._createCellWithInput(cellName, rowsCount + 1, rowsCount, colId);
                record[cellName] = rowsCount + 1;
            } else {
                $cell = this._createCellWithInput(cellName, '', rowsCount, colId);
                record[cellName] = '';
            }
            $row.appendChild($cell);
        });
        this.data.push(record);
        $row.appendChild(this._createCell(rowsCount));
        $tbody.appendChild($row);
        //this.emit(SimpleDataTable.EVENTS.UPDATE);
        // 设置最大的行数，判断行数是否等于最大行数
        if (this.data.length == this.maxRowCount) {
            this.render();
        }
    }

    _fetchColumnNames() {
        var arr = [];
        for (var i = 1; i <= 6; i++) {
            arr[i] = this.defaultColumnPrefix + i;
        }
        return arr;
    }

    setHeaders(items) {
        this.headers = items;
        return this;
    }

    load(data) {
        this.data = Array.from(data);
        return this;
    }

    emit(name, payload) {
        if (!this._events[name]) {
            return;
        }

        this._events[name].forEach((cb) => cb(payload));
        return this;
    }

    on(name, handler) {
        if (!this._events[name]) {
            this._events[name] = [];
        }

        this._events[name].push(handler);
        return this;
    }

    static clearElement($element) {
        while ($element.firstElementChild) {
            $element.firstElementChild.remove();
        }
    }
}

SimpleModelDataTable.EVENTS = {
    UPDATE: 'SimpleDataTable.EVENTS.UPDATE',
    ROW_REMOVED: 'SimpleDataTable.EVENTS.ROW_REMOVED',
};