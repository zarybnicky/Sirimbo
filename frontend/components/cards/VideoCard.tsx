import * as React from 'react';
import Link from 'next/link';
import { Video } from 'lib/data/use-videos';
import { Card } from 'components/Card';

export const VideoCard = ({ item: x }: { item: Video }) => {
  return <Link href={x.href} passHref>
    <a>
      <Card className="grid md:grid-cols-[1fr_3fr]">
        <div>
          <img src={x.img} alt={x.name} className="block w-full h-full object-cover" />
        </div>

        <div className="flex grow justify-stretch items-center px-4 py-2 md:pl-8 underline">
          <div className="font-bold">{x.name}</div>
        </div>
      </Card>
    </a>
  </Link>;
};
