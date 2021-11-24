import * as React from 'react';
import { Paper } from '@material-ui/core';

// https://morioh.com/p/7c097570ecd9
export const HighlightList = () => {
  const items = [{}, {}, {}, {}];
  return <React.Fragment>
    {items.map((x, i) => <HighlightItem key={i} item={x} />)}
  </React.Fragment>;
}
const HighlightItem = (props: { item: {}; key: number; }) => <Paper>{props.key}</Paper>
