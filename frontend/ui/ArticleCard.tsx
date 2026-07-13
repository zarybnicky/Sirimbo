import { RichTextView } from '@/ui/RichTextView';
import { buttonCls, cardCls } from '@/ui/style';
import { ChevronRight } from 'lucide-react';
import Image from 'next/image';
import Link, { LinkProps } from 'next/link';

interface Props {
  href: LinkProps['href'];
  img: string | null | undefined;
  header: string;
  preview: string;
  sizes: string;
  fallbackImage?: string;
  headingLevel?: 'h2' | 'h3';
}

export function ArticleCard(x: Props) {
  const Heading = x.headingLevel ?? 'h2';

  return (
    <Link href={x.href}>
      <div className={cardCls({ className: 'h-full flex flex-col group' })}>
        <div className="relative -m-3 mb-2 overflow-hidden h-[240px]">
          {(x.img || x.fallbackImage) && (
            <Image
              fill
              className="object-cover object-[50%_30%] transition-transform duration-300 group-hover:scale-110"
              src={x.img || x.fallbackImage!}
              alt={x.header}
              loading="lazy"
              fetchPriority="low"
              sizes={x.sizes}
            />
          )}
        </div>
        <Heading className="tracking-wide mt-2 text-lg text-accent-11 font-bold">
          {x.header}
        </Heading>
        <div className="mr-24 mt-1 mb-4 h-1 bg-accent-9" />

        <RichTextView className="text-neutral-12 grow" value={x.preview} />

        <div className="flex justify-center mt-3">
          <div className={buttonCls({ size: 'lg', variant: 'outline' })}>
            Zjisti více
            <ChevronRight />
          </div>
        </div>
      </div>
    </Link>
  );
}
