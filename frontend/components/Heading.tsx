import * as React from 'react';
import { Typography } from '@mui/material';
import { CellPlugin } from '@react-page/editor';
import { RGBColor } from 'react-color';

type HeadingProps = {
  color: string | RGBColor;
  image: string;
  text: string;
}

export const Heading = ({ color, image, text }: HeadingProps) => {
  const rgba = typeof color === 'string' ? color : `rgba(${color.r},${color.g},${color.b},${color.a})`;
  const gradient = `linear-gradient(${rgba},${rgba})`;
  const background = `${gradient}, url(${image}) no-repeat 50% 30%/cover`;

  return <div style={{ background }}>
    <div className="container mx-auto max-w-5xl">
      <div className="relative py-20">
        <Typography variant="h4" component="h2" sx={{
          zIndex: 3,
          color: 'white',
          textTransform: 'uppercase',
          fontWeight: 'bold',
        }}>{text}</Typography>

        <Typography variant="h3" component="div" sx={{
          position: 'absolute',
          top: '50%',
          left: 0,
          transform: 'translateY(-50%)',
          zIndex: 2,
          color: 'rgba(255, 255, 255, .4)',
          userSelect: 'none',
          textTransform: 'uppercase',
          fontWeight: 'bold',
        }}>{text}</Typography>
      </div>
    </div>
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
