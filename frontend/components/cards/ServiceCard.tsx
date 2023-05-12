import * as React from 'react';
import Link from 'next/link';
import classNames from 'classnames';
import { ArrowRight } from 'react-feather';
import { Route } from 'nextjs-routes';

type ServiceCardProps = {
  image: string;
  header: string;
  href?: Route | Exclude<Route, { query: any }>["pathname"];
  children: React.ReactNode;
};

export function ServiceCard(props: ServiceCardProps) {
  let content = (
    <>
      <div className="h-full relative">
        <div className="absolute block inset-0 overflow-hidden">
          <img
            className="sm:rounded-l-lg w-full h-full object-cover object-[50%_30%] transform transition duration-300 group-hover:scale-110"
            src={props.image}
            alt={props.header}
          />
        </div>
      </div>
      <div className="bg-stone-800 group-even:bg-red-500 w-3" />
      <div className="grow basis-4 px-4 py-6 md:px-8 md:py-8">
        <div className="text-2xl text-stone-800 group-odd:text-red-500 font-bold mb-2 md:mb-4">
          {props.header}
        </div>
        {props.children}
        {props.href && (
          <div>
            <span className="mt-4 button button-red">
              Zjistěte více <ArrowRight className="ml-2" />
            </span>
          </div>
        )}
      </div>
    </>
  );
  return (
    <Link
      href={props.href || ('#' as any as Route)}
      className={classNames(
        'group p-0 my-4 grid md:grid-cols-[1fr_4px_2fr]',
        'bg-white relative border border-stone-200 shadow-sm sm:rounded-lg p-3 mb-2',
      )}
    >
      {content}
    </Link>
  );
}
