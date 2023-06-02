import Link from 'next/link';
import { Route } from 'nextjs-routes';
import { CornerLeftUp as UpIcon } from 'lucide-react';

type TitleBarProps = {
  title: string;
  backHref?: Route | Exclude<Route, {query: any}>["pathname"];
  children?: React.ReactNode;
};

export function TitleBar({backHref, title, children,}: TitleBarProps) {
  return (
    <div className="my-4 lg:mt-8">
      {backHref && (
        <div className="block lg:hidden">
          <Link href={backHref} className="flex gap-2 text-red-500 items-center text-sm">
            <UpIcon className="inline-block w-4 h-4" /> Zpět na seznam
          </Link>
        </div>
      )}
      <div className="flex gap-2 flex-wrap grow-0 h-min justify-between items-start">
        <h4 className="text-2xl tracking-wide mb-4 order-2 w-full md:w-auto md:order-none first-letter:uppercase">
          {title}
        </h4>
        <div className="flex items-center flex-wrap gap-2">{children}</div>
      </div>
    </div>
  );
}
