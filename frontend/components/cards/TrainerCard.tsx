import * as React from 'react';
import { Card } from 'components/Card';

type TrainerCardProps = {
  image?: string;
  name: string;
}

export const TrainerCard = (props: TrainerCardProps & { children: React.ReactNode | React.ReactChildren; }) => {
  return <Card className="relative h-full p-0">
    <div className="bg-stone-800 text-white font-bold mb-0 p-4 border-l-8 border-red-500 header">
      <div className="font-bold mb-2">{props.name}</div>
    </div>
    <div className="avoid-trainer-pictures pt-0 p-4">
      {props.children}
    </div>
    {props.image && (
      <img className="w-[100px] h-[120px] object-cover absolute top-4 right-4" src={props.image} alt={props.name} />
    )}
  </Card>;
};
