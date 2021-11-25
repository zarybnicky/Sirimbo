import * as React from 'react';
import { Paper } from '@material-ui/core';

// https://morioh.com/p/7c097570ecd9
export const HighlightList = () => {
  const items = [{}, {}, {}, {}];
  return <React.Fragment>
    {items.map((x, i) => <HighlightItem key={i} item={{ i }} />)}
  </React.Fragment>;
}
const HighlightItem = (props: { item: {}; }) => <Paper>{props.item.toString()}</Paper>
