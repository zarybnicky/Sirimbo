import Link from 'next/link';
import { Route } from 'nextjs-routes';
import { CornerLeftUp as UpIcon } from 'lucide-react';
import { cn } from '@app/ui/cn';

type TitleBarProps = {
  title: string;
  backHref?: Route | Exclude<Route, {query: any}>["pathname"];
  children?: React.ReactNode;
  className?: string;
};

export function TitleBar({backHref, title, children,className}: TitleBarProps) {
  return (
    <div className={cn("my-4 lg:mt-8", className)}>
      {backHref && (
        <div className="block lg:hidden">
          <Link href={backHref} className="flex gap-2 text-accent-11 items-center text-sm">
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