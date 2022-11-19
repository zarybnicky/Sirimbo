import * as React from 'react';
import { CellPlugin } from '@react-page/editor';

type HeadingProps = {
  color: string | { r: number; g: number; b: number; a: number; };
  image: string;
  text: string;
}

export const Heading = ({ color, image, text }: HeadingProps) => {
  const rgba = typeof color === 'string' ? color : `rgba(${color.r},${color.g},${color.b},${color.a})`;
  const gradient = `linear-gradient(${rgba},${rgba})`;
  const background = `${gradient}, url(${image}) no-repeat 50% 30%/cover`;

  return <div className="col-full-width content mb-8" style={{ background }}>
    <div className="col-feature">
      <div className="relative py-20 mx-10 xl:mx-0">
        <h2 className="text-3xl drop-shadow-md mb-0 ml-1.5 z-3 text-white uppercase font-bold">
          {text}
        </h2>

        <div className="text-5xl absolute top-1/2 transform -translate-y-1/2 left-0 z-2 text-white/30 uppercase font-bold select-none">
          {text}
        </div>
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
    color: { r: 216, g: 28, b: 58, a: 0.6 },
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
