import * as React from 'react';
import Link from 'next/link';
import { Card } from 'components/Card';
import { Article } from 'lib/data/use-articles';

export const ArticleCard = ({ item: x }: { item: Article }) => {
  return <Link href={x.href} passHref>
    <a>
      <Card className="h-full flex flex-col">
        <div className="-m-3 mb-2">
          <img className="object-cover w-full" style={{ height: 240 }} src={x.img} title={x.header} />
        </div>
        <div className="tracking-wide mt-2 text-lg text-red-500 after:bg-red-500 font-bold">{x.header}</div>
        <div className="mr-24 mt-1 mb-4 h-1 bg-red-500" />

        <p className="text-slate-700 grow">{x.preview}</p>

        <div className="flex justify-center mt-3">
          <div className="button button-red button-lg">Více zde ᐳ</div>
        </div>
      </Card>
    </a>
  </Link>;
};
