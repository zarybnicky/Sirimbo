export function Item() { }

Item.Titlebar = function ItemTitleBar({ title, children }: {
  title?: string;
  children: React.ReactNode;
}) {
  return (
    <div className="flex grow-0 h-min justify-between">
      <div className="text-xl font-bold">{title}</div>
      <div>
        {children}
      </div>
    </div>
  );
}
