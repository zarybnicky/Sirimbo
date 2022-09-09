import * as React from 'react';
import { makeStyles, Container, Theme, Typography } from '@mui/material';
import { CellPlugin } from '@react-page/editor';
import { RGBColor } from 'react-color';

type HeadingProps = {
  color: string | RGBColor;
  image: string;
  text: string;
}

const useStyles = makeStyles<Theme, HeadingProps>(() => ({
  background: {
    background: ({ color, image }) => {
      const rgba = typeof color === 'string' ? color : `rgba(${color.r},${color.g},${color.b},${color.a})`;
      const gradient = `linear-gradient(${rgba},${rgba})`;
      return `${gradient}, url(${image}) no-repeat 50% 30%/cover`;
    },
  },
  heading: {
    position: 'relative',
    padding: '5rem 0',
  },
  bgText: {
    position: 'absolute',
    top: '50%',
    left: 0,
    transform: 'translateY(-50%)',
    zIndex: 2,
    color: 'rgba(255, 255, 255, .4)',
    userSelect: 'none',
    textTransform: 'uppercase',
    fontWeight: 'bold',
  },
  fgText: {
    zIndex: 3,
    color: 'white',
    textTransform: 'uppercase',
    fontWeight: 'bold',
  },
}));

export const Heading = (props: HeadingProps) => {
  const classes = useStyles(props);
  return <div className={classes.background}>
    <Container maxWidth="lg">
      <div className={classes.heading}>
        <Typography variant="h4" component="h2" className={classes.fgText}>{props.text}</Typography>
        <Typography variant="h3" component="div" className={classes.bgText}>{props.text}</Typography>
      </div>
    </Container>
  </div>;
};

export const HeadingPlugin: CellPlugin<HeadingProps> = {
  Renderer: ({ data }) => <Heading {...data} />,

  id: 'app-heading-plugin',
  title: 'Heading',
  description: undefined,
  version: 1,
  createInitialData: () => ({
    text: '...nadpis',
    color: 'rgba(216, 28, 58, 0.6)',
    image: '/images/mohelnice2021-title.jpg',
  }),
  controls: {
    type: 'autoform',
    schema: {
      required: [],
      properties: {
        text: { type: 'string' },
        image: { type: 'string' },
        color: {
          type: 'string',
          default: 'black',
          uniforms: {
            label: 'Barva překryvu (chybí chytrý výběr barvy, nefunguje)',
            /* component: ColorPickerField, */
          },
        },
      },
    },
  },
};
