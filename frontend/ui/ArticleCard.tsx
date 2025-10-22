import { RichTextView } from '@/ui/RichTextView';
import { buttonCls, cardCls } from '@/ui/style';
import { ChevronRight } from 'lucide-react';
import Image from 'next/image';
import Link, { LinkProps } from 'next/link';
import * as React from 'react';

interface Props {
  href: LinkProps['href'];
  img: string | undefined;
  header: string;
  preview: string;
}

export function ArticleCard(x: Props) {
  return (
    <Link href={x.href}>
      <div className={cardCls({ className: "h-full flex flex-col group" })}>
        <div className="relative -m-3 mb-2 overflow-hidden h-[240px]">
          {x.img && (
            <Image
              fill
              className="object-cover object-[50%_30%] transition duration-300 group-hover:scale-110"
              src={x.img}
              alt={x.header}
              sizes="(max-width: 768px) 100vw, (max-width: 1200px) 30vw, 25vw"
            />
          )}
        </div>
        <div className="tracking-wide mt-2 text-lg text-accent-9 font-bold">
          {x.header}
        </div>
        <div className="mr-24 mt-1 mb-4 h-1 bg-accent-9" />

        <RichTextView className="text-neutral-12 grow" value={x.preview} />

        <div className="flex justify-center mt-3">
          <div className={buttonCls({ size: 'lg' })}>
            Zjisti více
            <ChevronRight />
          </div>
        </div>
      </div>
    </Link>
  );
};
