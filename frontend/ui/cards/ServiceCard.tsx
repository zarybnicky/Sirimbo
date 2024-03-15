import * as React from 'react';
import Link, { LinkProps } from 'next/link';
import { ArrowRight } from 'lucide-react';
import Image from 'next/image';
import { buttonCls } from '@/ui/style';
import { cn } from '../cn';

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
          className="sm:rounded-l-lg object-cover object-[50%_30%] transition duration-300 group-hover:scale-110"
          src={props.image}
          alt={props.header}
          sizes="(max-width: 768px) 100vw, (max-width: 1200px) 30vw, 25vw"
          fill
        />
      </div>
      <div className="bg-neutral-9 group-even:bg-accent-9 h-3 md:h-auto md:w-3" />
      <div className="grow basis-4 px-4 py-6 md:p-8">
        <div className="text-2xl text-neutral-11 group-odd:text-accent-11 font-bold mb-2 md:mb-4">
          {props.header}
        </div>
        <div className="min-h-[3.5rem]">
          {props.children}
        </div>
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
