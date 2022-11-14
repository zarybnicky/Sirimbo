import * as React from 'react';
import { CellPlugin } from '@react-page/editor';
import { defaultSlate } from '../SlateReadonly';
import { Card } from 'components/Card';

type ServiceCardProps = {
  image: string;
  header: string;
}

export const ServiceCard = (props: ServiceCardProps & { children: React.ReactNode | React.ReactChildren; }) => {
  return <Card className="group p-0 my-8 grid sm:grid-cols-[1fr_4px_2fr]">
    <div className="w-full h-full">
      <img className="w-full h-full object-cover" src={props.image} alt={props.header} />
    </div>
    <div className="bg-slate-800 group-even:bg-red-500 w-4" />
    <div className="grow basis-4 px-4 py-6 md:px-8 md:py-12">
      <div className="font-lg text-slate-800 group-odd:text-red-500 font-bold mb-2 md:mb-4">{props.header}</div>
      {props.children}
    </div>
  </Card >;
};

export const ServiceCardPlugin: CellPlugin<ServiceCardProps> = {
  Renderer: ({ children, data }) => <ServiceCard {...data}>
    {children}
  </ServiceCard>,

  id: 'app-service-card-plugin',
  title: 'ServiceCard',
  version: 1,
  createInitialData: () => ({
    header: 'Tréninkový program',
    image: '/images/services-pripravka.png',
  }),
  createInitialChildren: () => [[{ plugin: defaultSlate }]],
  childConstraints: {
    maxChildren: 1,
  },
  controls: {
    type: 'autoform',
    schema: {
      required: [],
      properties: {
        header: { type: 'string' },
        image: { type: 'string' },
      },
    },
  },
};
