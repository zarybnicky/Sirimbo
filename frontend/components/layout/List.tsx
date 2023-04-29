import classNames from 'classnames';
import Link from 'next/link';

export function List({ children }: { children: React.ReactNode }) {
  return (
    <div
      className={classNames(
        'flex flex-col px-2 max-h-screen min-h-screen flex-none w-full lg:w-80 xl:w-96',
        'border-r dark:border-gray-800 lg:border-stone-150 lg:bg-stone-50 lg:dark:bg-gray-900',
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
      <div className="font-bold">{title}</div>
      {children}
    </div>
  );
}

function ListTitleButton({
  active,
  icon: Icon,
  children,
  href,
  onClick,
}: {
  children: React.ReactNode;
  active?: boolean;
  icon?: React.ElementType<{ className?: string }>;
  href?: string;
  onClick?: () => void;
}) {
  const cx = classNames(
    'shadow-md inline-flex items-center gap-1 px-3 rounded-2xl py-1 text-xs tracking-tight font-bold',
    active ? 'tracking-wide bg-red-500 text-white' : 'bg-white hover:bg-stone-50',
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
  href: string;
  title: React.ReactNode;
  subtitle?: React.ReactNode;
  className?: string;
}) {
  return (
    <Link
      key={href}
      href={href}
      className={classNames(
        'relative p-2 mr-2 my-1 ml-0 rounded-lg grid',
        active ? 'font-semibold bg-red-500 text-white shadow-md' : 'hover:bg-stone-200',
        className,
      )}
    >
      <div>{title}</div>
      <div
        className={classNames('text-sm', active ? 'text-stone-200' : 'text-stone-500')}
      >
        {subtitle}
      </div>
      {children}
    </Link>
  );
}

function ListScroll({ children }: { children: React.ReactNode }) {
  return (
    <div
      className={classNames(
        'scrollbar-thin scrollbar-track-transparent scrollbar-thumb-stone-800/20 hover:scrollbar-thumb-stone-800/50',
        'relative h-full overflow-y-auto',
      )}
    >
      {children}
    </div>
  );
}

List.TitleBar = ListTitleBar;
List.TitleButton = ListTitleButton;
List.Item = ListItem;
List.Scroll = ListScroll;
