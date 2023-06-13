import * as React from 'react';
import Link from 'next/link';
import { Card } from 'components/Card';
import { Article } from 'lib/data/use-articles';
import { ChevronRight } from 'lucide-react';

export const ArticleCard = ({ item: x }: { item: Article }) => {
  return (
    <Link href={x.href}>
      <Card className="h-full flex flex-col group">
        <div className="-m-3 mb-2 overflow-hidden">
          <img
            className="object-cover object-[50%_30%] w-full transform transition duration-300 group-hover:scale-110"
            style={{ height: 240 }}
            src={x.img}
            title={x.header}
          />
        </div>
        <div className="tracking-wide mt-2 text-lg text-red-500 font-bold">
          {x.header}
        </div>
        <div className="mr-24 mt-1 mb-4 h-1 bg-red-500" />

        <p className="text-stone-700 grow">{x.preview}</p>

        <div className="flex justify-center mt-3">
          <div className="font-bold tracking-wider button button-lg button-accent">
            Zjisti více
            <ChevronRight className="h-3 w-3 ml-2 -mr-2" />
          </div>
        </div>
      </Card>
    </Link>
  );
};
