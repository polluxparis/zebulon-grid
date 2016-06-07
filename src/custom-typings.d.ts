// declaration types for react-virtualized

declare module "react-virtualized"{

  interface GridProps{
    width: number,
    height: number,
    columnWidth:number,
    rowHeight:number,
    columnCount:number,
    rowCount:number,
    onScroll?: any,
    scrollTop?: number,
    scrollLeft?: number,
    overscanRowCount?:number,
    overscanColumnCount?:number,
    cellRenderer?:(cellRendererArgs) => (JSX.Element|string),
    cellRangeRenderer?:(cellRangeRendererArgs) => (JSX.Element[]|string[])
  }

  interface cellRendererArgs {
    columnIndex:number,
    rowIndex:number
  }


  interface CellSizeAndPositionManagerConstructorParams{
    cellCount: number,
    cellSizeGetter: Function,
    estimatedCellSize: number
  }

  interface ConfigureParams{
    cellCount: number,
    estimatedCellSize: number
  }

  interface GetVisibleCellRangeParams{
    containerSize: number,
    offset: number
  }

  interface SizeAndPositionData{
    offset: number,
    size: number
  }

  interface VisibleCellRange{
    start?: number,
    stop?: number
  }

  interface cellSizeAndPositionGetter {
    index:number
  }

  interface ScrollSyncProps{
    // children:({onScroll, scrollLeft, scrollTop}) =>JSX.Element
  }

  interface CollectionProps{
    width: number,
    height: number,
    cellCount: number,
    onScroll?: any,
    scrollTop?: number,
    scrollLeft?: number,
    cellRenderer:(cellRendererArgs) => (JSX.Element|string),
    cellSizeAndPositionGetter:(cellSizeAndPositionGetter) => { height: number, width: number, x: number, y: number }
  }

  class Grid extends __React.Component<GridProps,{}>{}

  class ScrollSync extends __React.Component<ScrollSyncProps,{}>{}

  class Collection extends __React.Component<CollectionProps,{}>{}

  class AutoSizer  extends __React.Component<any,{}>{}

  class VirtualScroll extends __React.Component<any,{}>{}

  interface cellRangeRendererArgs{
    cellCache: Object,
    cellRenderer: Function,
    columnSizeAndPositionManager: CellSizeAndPositionManager,
    columnStartIndex: number,
    columnStopIndex: number,
    isScrolling: boolean,
    rowSizeAndPositionManager: CellSizeAndPositionManager,
    rowStartIndex: number,
    rowStopIndex: number,
    scrollLeft: number,
    scrollTop: number
  }

  class CellSizeAndPositionManager {}
}

declare module "react-dropdown"{
  export default class Dropdown extends __React.Component<any,{}>{}
}

declare module "react-resizable"{
  class ResizableBox extends __React.Component<any,{}>{}
}
