import * as React from 'react';
import Link from 'next/link';
import classNames from 'classnames';
import { ArrowRight } from 'lucide-react';
import { Route } from 'nextjs-routes';

type ServiceCardProps = {
  image: string;
  header: string;
  href?: Route | Exclude<Route, { query: any }>["pathname"];
  children: React.ReactNode;
};

export function ServiceCard(props: ServiceCardProps) {
  return (
    <Link
      href={props.href || ('#' as any as Route)}
      className={classNames(
        'group p-0 my-4 flex flex-col md:grid md:grid-cols-[1fr_4px_2fr]',
        'bg-white relative border border-stone-200 shadow-sm sm:rounded-lg mb-2',
      )}
    >
      <div className="relative overflow-hidden max-h-[300px]">
        <img
          className="sm:rounded-l-lg w-full h-full object-cover object-[50%_30%] transition duration-300 group-hover:scale-110"
          src={props.image}
          alt={props.header}
        />
      </div>
      <div className="bg-stone-800 group-even:bg-red-500 h-3 md:h-auto md:w-3" />
      <div className="grow basis-4 px-4 py-6 md:p-8">
        <div className="text-2xl text-stone-800 group-odd:text-red-500 font-bold mb-2 md:mb-4">
          {props.header}
        </div>
        <div className="min-h-[3.5rem]">
          {props.children}
        </div>
        {props.href && (
          <div>
            <span className="mt-4 button button-accent">
              Zjisti více <ArrowRight className="ml-2" />
            </span>
          </div>
        )}
      </div>
    </Link>
  );
}
