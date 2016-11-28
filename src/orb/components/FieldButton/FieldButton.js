import React, { PureComponent } from 'react';
import { findDOMNode } from 'react-dom';
import { DragSource } from 'react-dnd';
import { Overlay } from 'react-overlays';

import FilterPanel from '../../containers/FilterPanel';

const filterImage = 'url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAYAAACprHcmAAAAMUlEQVQYlWP4//9/I7GYgSzFDHgAVsX/sQCsirFpQFaI1c0wDegKB0AxeihQFs7EYAAT8WYwzt7jxgAAAABJRU5ErkJggg==) no-repeat 0px 0px';

class FieldButton extends PureComponent {
  constructor(props) {
    super(props);
    this.state = { filtering: false };

    this.addFilterPanel = this.addFilterPanel.bind(this);
    this.removeFilterPanel = this.removeFilterPanel.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  componentDidMount() {
    this.DOMNode = findDOMNode(this.ref);
    this.buttonDOMNode = findDOMNode(this.buttonRef);
  }

  removeFilterPanel() {
    this.setState({ filtering: false });
  }

  addFilterPanel(e) {
    if (!this.state.filtering) {
      this.setState({ filtering: true });
    }
    e.preventDefault();
  }

  handleClick() {
    const { sort, field } = this.props;
    sort(field.id);
  }

  render() {
    const { field, connectDragSource, isDragging } = this.props;
    const { filtering } = this.state;
    const styles = {
      div: {
        width: isDragging ? 0 : '',
        backgroundColor: '#5bc0de',
        borderRadius: 4,
        cursor: 'default',
        opacity: isDragging ? 0 : 1,
        padding: '0.2em',
        marginTop: '0.2em',
        marginBottom: '0.2em',
        display: 'flex',
        justifyContent: 'space-between',
        border: 'none',
        outline: 'none',
      },
      filterButton: {
        background: filterImage,
        border: 'none',
        outline: 'none',
        width: 11,
        height: 11,
        marginLeft: '0.5em',
      },
    };
    return connectDragSource(
      <div id={field.caption} ref={(input) => { this.ref = input; }}>
        <div style={styles.div}>
          <div onClick={this.handleClick}>
            {field.caption}
          </div>
          <button
            ref={(input) => { this.buttonRef = input; }}
            onClick={this.addFilterPanel}
            style={styles.filterButton}
          />
        </div>
        <Overlay
          container={document.body}
          placement="right"
          show={filtering}
          target={this.buttonDOMNode}
          rootClose
          onHide={this.removeFilterPanel}
        >
          <FilterPanel
            field={field}
            onHide={this.removeFilterPanel}
          />
        </Overlay>
      </div>);
  }
}

const fieldSource = {
  beginDrag(props) {
    return {
      id: props.field.id,
      axetype: props.axetype,
    };
  },
};

function collect(connect, monitor) {
  return {
    connectDragSource: connect.dragSource(),
    isDragging: monitor.isDragging(),
  };
}

export default DragSource('button', fieldSource, collect)(FieldButton);
