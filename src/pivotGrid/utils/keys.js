import { KEY_SEPARATOR, AXIS_SEPARATOR, TOTAL_ID } from '../constants';
import { HeaderType } from '../Cells';

export function getKey({
  headerType,
  parent,
  datafieldId,
  crossAxisFieldsCode,
  value,
  dimension
}) {
  switch (headerType) {
    case HeaderType.DATA_HEADER:
      if (parent.type === HeaderType.GRAND_TOTAL) {
        // If the parent is a Total header, split to include the datafield id
        // on the right side of the axis separator
        const [totalID, crossAxisFieldsCode] = parent.key.split(AXIS_SEPARATOR);
        return `${totalID}${KEY_SEPARATOR}${datafieldId}${AXIS_SEPARATOR}${crossAxisFieldsCode}`;
      }
      return `${parent.key}${KEY_SEPARATOR}${datafieldId}`;
    case HeaderType.GRAND_TOTAL:
      return `${TOTAL_ID}${AXIS_SEPARATOR}${crossAxisFieldsCode.join(KEY_SEPARATOR)}`;
    case HeaderType.SUB_TOTAL:
      return parent ? `${parent.key}${KEY_SEPARATOR}${value}` : value;
    case HeaderType.INNER:
    case HeaderType.WRAPPER:
      return parent
        ? `${parent.key}${KEY_SEPARATOR}${dimension.id}`
        : String(dimension.id);
    default:
      throw new Error(`Header type ${headerType} is unknown`);
  }
}

export function getCellInfosKey(cellInfos) {
  const columnInfos = cellInfos.dimensions
    .filter(dim => dim.axis === 'columns')
    .sort((dim1, dim2) => dim1.index - dim2.index)
    .map(dim => dim.cell.id);
  const rowInfos = cellInfos.dimensions
    .filter(dim => dim.axis === 'rows')
    .sort((dim1, dim2) => dim1.index - dim2.index)
    .map(dim => dim.cell.id);
  let columns;
  let rows;
  if (columnInfos.length === 0 && rowInfos.length === 0) {
    if (cellInfos.measure.axis === 'columns') {
      columns = getKey({
        headerType: HeaderType.DATA_HEADER,
        parent: { type: HeaderType.GRAND_TOTAL },
        datafieldId: cellInfos.measure.id,
        crossAxisFieldsCode: [TOTAL_ID]
      });
      rows = getKey({
        headerType: HeaderType.GRAND_TOTAL,
        crossAxisFieldsCode: [TOTAL_ID]
      });
    } else {
      rows = getKey({
        headerType: HeaderType.DATA_HEADER,
        parent: { type: HeaderType.GRAND_TOTAL },
        datafieldId: cellInfos.measure.id,
        crossAxisFieldsCode: [TOTAL_ID]
      });
      columns = getKey({
        headerType: HeaderType.GRAND_TOTAL,
        crossAxisFieldsCode: [TOTAL_ID]
      });
    }
  } else if (rowInfos.length === 0) {
    columns = columnInfos.join(KEY_SEPARATOR);
    const crossAxisFieldsCode = cellInfos.dimensions
      .filter(dim => dim.axis === 'columns')
      .sort((dim1, dim2) => dim1.index - dim2.index)
      .map(dim => dim.dimension.id);
    if (cellInfos.measure.axis === 'columns') {
      columns = `${columns}${KEY_SEPARATOR}${cellInfos.measure.id}`;
      rows = getKey({
        headerType: HeaderType.GRAND_TOTAL,
        crossAxisFieldsCode
      });
    } else {
      rows = getKey({
        headerType: HeaderType.DATA_HEADER,
        parent: {
          type: HeaderType.GRAND_TOTAL,
          key: getKey({
            headerType: HeaderType.GRAND_TOTAL,
            crossAxisFieldsCode
          })
        },
        datafieldId: cellInfos.measure.id,
        crossAxisFieldsCode: [TOTAL_ID]
      });
    }
  } else if (columnInfos.length === 0) {
    rows = rowInfos.join(KEY_SEPARATOR);
    const crossAxisFieldsCode = cellInfos.dimensions
      .filter(dim => dim.axis === 'rows')
      .sort((dim1, dim2) => dim1.index - dim2.index)
      .map(dim => dim.dimension.id);
    if (cellInfos.measure.axis === 'rows') {
      rows = `${rows}${KEY_SEPARATOR}${cellInfos.measure.id}`;
      columns = getKey({
        headerType: HeaderType.GRAND_TOTAL,
        crossAxisFieldsCode
      });
    } else {
      columns = getKey({
        headerType: HeaderType.DATA_HEADER,
        parent: {
          type: HeaderType.GRAND_TOTAL,
          key: getKey({
            headerType: HeaderType.GRAND_TOTAL,
            crossAxisFieldsCode
          })
        },
        datafieldId: cellInfos.measure.id
      });
    }
  } else {
    columns = columnInfos.join(KEY_SEPARATOR);
    rows = rowInfos.join(KEY_SEPARATOR);
    if (cellInfos.measure.axis === 'columns') {
      columns = `${columns}${KEY_SEPARATOR}${cellInfos.measure.id}`;
    } else {
      rows = `${rows}${KEY_SEPARATOR}${cellInfos.measure.id}`;
    }
  }

  return { columns, rows };
}

export function getNextKey(current, next) {
  const firstLeafHeader =
    current.firstHeaderRow[current.firstHeaderRow.length - 1];
  const keys = firstLeafHeader.key.split(KEY_SEPARATOR);
  let nextKey = '';
  if (current.fields.length > next.fields.length) {
    const nextFieldIds = next.fields.map(field => field.id);
    const missingFieldPosition = current.fields.findIndex(
      field => !nextFieldIds.includes(field.id)
    );
    nextKey = keys.slice(0, missingFieldPosition).join(KEY_SEPARATOR);
  } else if (current.fields.length < next.fields.length) {
    const previousFieldIds = current.fields.map(field => field.id);
    const newFieldPosition = next.fields.findIndex(
      field => !previousFieldIds.includes(field.id)
    );
    nextKey = keys.slice(0, newFieldPosition).join(KEY_SEPARATOR);
  } else if (current.dataFieldsCount !== next.dataFieldsCount) {
    // A data field has been toggled
    nextKey = keys.slice(0, -1).join(KEY_SEPARATOR);
  } else {
    // A filter has been modified
    // For the moment, do nothing
    nextKey = '';
  }
  return nextKey;
}
