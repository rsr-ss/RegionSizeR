class SimpleDataTable {
    constructor($el, options = {}) {
        this.$el = $el;
        if (options.withAddButton == false) this.withAddButton = false;
        else this.withAddButton = true;
        this.addButtonLabel = options.addButtonLabel || '✚';
        if (options.withRemoveButton == false) this.withRemoveButton = false;
        else this.withRemoveButton = true;
        this.readonly = options.readonly || false;
        this.keyColumnReadonly = options.keyColumnReadonly || 1;
        this.defaultColumnPrefix = options.defaultColumnPrefix || 'column';
        this.defaultColumnNumber = options.defaultColumnNumber || null;
        this.defaultHighlightedCellClass = options.defaultHighlightedCellClass || 'highlighted-cell';
        this.headers = [];
        this.data = [];
        this._events = {};
        this.colWidth = options.colWidth;
        this.maxRowCount = options.maxRowCount || 3;
    }

    _renderTHead($table) {
        const $thead = document.createElement('thead');
        const $row = document.createElement('tr');
        this.headers.forEach((name, index) => {
            const $cell = this._createEmptyHeaderCell(this.colWidth[index]);
            if (name == 'ID') $cell.textContent = 'No.';
            else $cell.textContent = name;
            $row.appendChild($cell);
        });

        $thead.appendChild($row);
        $table.appendChild($thead);
    }

    //render时执行,table的数据部分
    _renderTBody($table) {
        const $tbody = document.createElement('tbody');
        
        //console.log('!!!Simple-data-table!!!: $tbody:');
        //console.log($tbody);
  
        this.data.forEach((item, rowIndex) => {
            const $row = document.createElement('tr');

            Object.entries(item).forEach(([key, value], colIndex) => {
                //console.log('_renderTBody ');
                //console.log('name: ' + key + ', value: ' + value + ', row: '+ rowIndex + ', col: ' + colIndex);
                if (key == this.defaultColumnPrefix + 1) value = rowIndex + 1;
                const $cell = this._createCellWithInput(key, value, rowIndex, colIndex);
                $row.appendChild($cell);
            });

            const $cell = this.readonly
                ? this._createEmptyCell()
                : this._createCell(rowIndex);
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
        $wrapper.classList.add('simple-data-table');
        const $table = document.createElement('table');
        $table.className = "simple-data-table";

        if (this.headers.length > 0) {
            this._renderTHead($table);
        }

        this._renderTBody($table);

        $wrapper.appendChild($table);

        // 判断是否需要添加add button
        if (!this.readonly && this.withAddButton && this.getRowsCount() < this.maxRowCount) {
            const $addButton = this._createAddButton();
            $wrapper.appendChild($addButton);
        }

        this.$el.appendChild($wrapper);
        return this;
    }

    getRowsCount() {
        //return this.$el.querySelectorAll('tr').length;
        return this.data.length;
    }
    
    getColumnTotal(columnName) {
        let total = 0;
        for (var i = 0; i < this.data.length; i++) {
            if (parseInt(this.data[i][columnName]).toString() != 'NaN') {
                total += parseInt(this.data[i][columnName]);
            }
        }
        //console.log('getColumnTotal: total = ' + total);
        return total;
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

    _createEmptyHeaderCell(width) {
        const $cell = document.createElement('th');
        $cell.style.width = width + "%";
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
        
        this.data.forEach((item, rowIndex) => {
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

    _createCellWithInput(name, value, rowIndex, colIndex) {
        const $cell = document.createElement('td');
        const $input = document.createElement('input');
        $input.value = value;
        $input.name = name;

        if (this.readonly) {
            $input.disabled = true;
        }
        
        if (colIndex <= this.keyColumnReadonly - 1) {
            $input.disabled = true;
        }

        $input.addEventListener('change', () => {
            this.data[rowIndex][name] = $input.value;
            this.emit(SimpleDataTable.EVENTS.UPDATE, this.data);
        });
        
        //给空单元格加底纹
        //if ($input.value.length == 0) {
        //    $cell.classList.add(this.defaultHighlightedCellClass);
        //}

        $cell.appendChild($input);
        //console.log('_createCellWithInput: $cell:' )
        //console.log($cell);
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
        // colId从0开始
        columnNames.forEach((cellName, colId) => {
            //console.log('_createEmptyRow:');
            //console.log('cellName: ' + cellName + ', colId: ' + colId);
            let $cell;
            if (colId == 0) {
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
        if (this.getRowsCount() == this.maxRowCount) {
            this.render();
        }
    }

    _fetchColumnNames() {
        const $tbody = this.$el.querySelector('tbody');
        const $firstRecord = $tbody.querySelector('tr');

        if (!$firstRecord) {
            const size = this.defaultColumnNumber
                ? this.defaultColumnNumber
                : this.headers
                    ? this.headers.length
                    : this.data[0] && this.data[0].length;
            if (!size) {
                return [];
            }
            return Array(size)
                .fill(this.defaultColumnPrefix)
                .map((name, index) => `${name}${index + 1}`);
        }

        const $elements = Array.from($firstRecord.children);
        return $elements
            .map(($cell) => $cell.querySelector('input'))
            .filter(($element) => $element)
            .map(($input) => $input.name);
    }

    setHeaders(items) {
        this.headers = items;
        return this;
    }

    load(data) {
        this.data = Array.from(data);
        this.emit(SimpleDataTable.EVENTS.UPDATE, this.data);
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

SimpleDataTable.EVENTS = {
    UPDATE: 'SimpleDataTable.EVENTS.UPDATE',
    ROW_REMOVED: 'SimpleDataTable.EVENTS.ROW_REMOVED',
};

// Exports
if (typeof module === 'object' && module.exports) {
    module.exports = { SimpleDataTable };
} else if (typeof define === 'function' && define.amd) {
    define(() => ({ SimpleDataTable }));
} else {
    window.SimpleDataTable = SimpleDataTable;
}


