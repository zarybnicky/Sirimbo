import classNames from 'classnames';
import Link from 'next/link';
import { Route } from 'nextjs-routes';

export function List({ children }: { children: React.ReactNode }) {
  return (
    <div
      className={classNames(
        'flex flex-col px-2 max-h-screen min-h-screen flex-none w-full lg:w-80 xl:w-96',
        'border-r lg:border-accent-6 lg:bg-accent-3 dark:lg:bg-accent-4',
      )}
    >
      {children}
    </div>
  );
}

function ListTitleBar({
  title,
  children,
}: {
  title: React.ReactNode;
  children?: React.ReactNode;
}) {
  return (
    <div className="px-1 py-4 flex items-center justify-between flex-wrap">
      <div className="font-bold first-letter:uppercase">{title}</div>
      {children}
    </div>
  );
}

type ListTitleButtonProps = {
  children: React.ReactNode;
  active?: boolean;
  icon?: React.ElementType<{ className?: string }>;
  href?: Route | Exclude<Route, { query: any }>['pathname'];
  onClick?: () => void;
};

function ListTitleButton({ active, icon: Icon, children, href, onClick }: ListTitleButtonProps) {
  const cx = classNames(
    'shadow-md inline-flex items-center gap-1 px-3 rounded-2xl py-1 text-xs tracking-tight font-bold',
    active ? 'bg-primary text-white' : 'bg-neutral-4 hover:bg-neutral-5',
  );
  return href ? (
    <Link href={href} className={cx}>
      {Icon && <Icon className="w-4" />} {children}
    </Link>
  ) : (
    <button className={cx} onClick={onClick}>
      {Icon && <Icon className="w-4" />} {children}
    </button>
  );
}

function ListItem({
  className,
  active,
  children,
  href,
  title,
  subtitle,
}: {
  children?: React.ReactNode;
  active?: boolean;
  href: Route | Exclude<Route, { query: any }>['pathname'];
  title: React.ReactNode;
  subtitle?: React.ReactNode;
  className?: string;
}) {
  return (
    <Link
      key={JSON.stringify(href)}
      href={href}
      className={classNames(
        'relative p-2 pl-5 mr-2 my-1 rounded-lg grid',
        active ? 'font-semibold bg-primary text-white shadow-md' : 'hover:bg-neutral-4',
        className,
      )}
    >
      <div>{title}</div>
      <div className={classNames('text-sm', active ? 'text-white' : 'text-neutral-11')}>
        {subtitle}
      </div>
      {children}
    </Link>
  );
}

List.TitleBar = ListTitleBar;
List.TitleButton = ListTitleButton;
List.Item = ListItem;
