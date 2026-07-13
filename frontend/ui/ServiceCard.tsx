import { cn } from '@/lib/cn';
import { buttonCls } from '@/ui/style';
import { ArrowRight } from 'lucide-react';
import Image from 'next/image';
import Link, { type LinkProps } from 'next/link';
import React from 'react';

type ServiceCardProps = {
  image: string;
  header: string;
  href?: LinkProps['href'];
  children: React.ReactNode;
};

export function ServiceCard(props: ServiceCardProps) {
  return (
    <Link
      href={props.href || { hash: '#' }}
      className={cn(
        'group p-0 my-4 flex flex-col md:grid md:grid-cols-[1fr_4px_2fr]',
        'bg-neutral-0 relative border border-neutral-3 shadow-sm sm:rounded-lg mb-2',
      )}
    >
      <div className="relative overflow-hidden min-h-[300px]">
        <Image
          className="sm:rounded-l-lg object-cover object-[50%_30%] transition-transform duration-300 group-hover:scale-110"
          src={props.image}
          alt={props.header}
          fill
          loading="lazy"
          fetchPriority="low"
          sizes="(min-width: 768px) 300px, calc(100vw - 1rem)"
        />
      </div>
      <div className="bg-neutral-9 group-even:bg-accent-9 h-3 md:h-auto md:w-3" />
      <div className="grow basis-4 px-4 py-6 md:p-8">
        <h2 className="text-2xl text-neutral-11 group-odd:text-accent-11 font-bold mb-2 md:mb-4">
          {props.header}
        </h2>
        <div className="min-h-14">{props.children}</div>
        {props.href && (
          <div>
            <span className={buttonCls({ className: 'mt-4' })}>
              Zjisti více <ArrowRight />
            </span>
          </div>
        )}
      </div>
    </Link>
  );
}
