window.onload = function() {

  var extractDataFromRow = function(tr, className) {
    var td = tr.querySelector('td.' + className);
    var a = td.querySelector('a');
    if (a) {
      return a.innerHTML;
    } else {
      return td.innerHTML;
    }
  };

  var removeRows = function(table, rows) {
    for (var i = 0; i < rows.length; i++) {
      table.removeChild(rows[i]);
    }
  };

  var sortTable = function(className, compare) {
    var table = document.querySelector('table.file-list tbody');
    var rows = Array.prototype.slice.call(table.querySelectorAll('tr'), 0);
    removeRows(table, rows);

    rows.sort(function(a, b) {
      var aActual = extractDataFromRow(a, className);
      var bActual = extractDataFromRow(b, className);
      return compare(aActual, bActual);
    });
    for (var i = 0; i < rows.length; i++) {
      rows[i].className = 'file-info' + (((i % 2) == 0) ? ' stripe' : '');
      table.appendChild(rows[i]);
    }
  };

  var oppositeArrowDirection = function(dir) {
    if (dir === 'up') {
      return 'down';
    } else {
      return 'up';
    }
  };

  var createSorter = function(className, arrowDirection, compare, negativeCompare) {
    var sortClass = 'th.' + className + ' div.sort-icon-' + arrowDirection;
    var element = document.querySelector(sortClass);
    var clickHandler = function() {
      var oppArrowDirection = oppositeArrowDirection(arrowDirection);
      sortTable(className, compare);
      element.removeEventListener('click', clickHandler);
      element.className = 'sort-icon-' + oppArrowDirection;
      createSorter(className, oppArrowDirection, negativeCompare, compare);
    };
    element.addEventListener('click', clickHandler);
  };

  var stringCompareAsc = function(a, b) { return a.localeCompare(b); };
  var stringCompareDesc = function(a, b) { return b.localeCompare(a); };
  var floatCompareAsc = function(a, b) { return parseFloat(a) - parseFloat(b); };
  var floatCompareDesc = function(a, b) { return parseFloat(b) - parseFloat(a); };

  createSorter('file-name', 'up', stringCompareDesc, stringCompareAsc);
  createSorter('coverage-percentage', 'up', floatCompareAsc, floatCompareDesc);
  createSorter('covered-expressions', 'up', floatCompareAsc, floatCompareDesc);
  createSorter('uncovered-expressions', 'up', floatCompareAsc, floatCompareDesc);
  createSorter('total-expressions', 'up', floatCompareAsc, floatCompareDesc);
  sortTable('coverage-percentage', floatCompareAsc);
};
