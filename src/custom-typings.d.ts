// declaration types for react-virtualized
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
  cellRenderer:(cellRendererArgs) => (JSX.Element|string)
}

interface cellRendererArgs {
  columnIndex:number,
  rowIndex:number
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

declare module "react-virtualized"{
  class Grid extends __React.Component<GridProps,{}>{}

  class ScrollSync extends __React.Component<ScrollSyncProps,{}>{}

  class Collection extends __React.Component<CollectionProps,{}>{}

  class AutoSizer  extends __React.Component<any,{}>{}
}
