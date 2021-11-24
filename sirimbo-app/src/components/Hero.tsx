import * as React from 'react';
import { Container, Button } from '@material-ui/core';
import Carousel from 'react-material-ui-carousel'

export const HeroArticles = () => <Container maxWidth="lg">
  <Carousel>{items.map((item, i) => <HeroItem key={i} item={item} />)}</Carousel>;
</Container>;

// https://www.npmjs.com/package/react-material-ui-carousel
const items = [
  {
    name: "Random Name #1",
    description: "Probably the most random thing you have ever seen!"
  },
  {
    name: "Random Name #2",
    description: "Hello World!"
  }
];

const HeroItem = (props: { item: { name: string; description: string; } }) => <div>
  <h2>{props.item.name}</h2>
  <p>{props.item.description}</p>
  <Button className="CheckButton">Check it out!</Button>
</div>;
