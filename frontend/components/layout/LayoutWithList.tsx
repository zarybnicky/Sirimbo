export const ListDetailView = ({ list, children, hasDetail }: {
  list: React.ReactNode;
  children: React.ReactNode;
  hasDetail?: boolean;
}) => (
  <div className="flex w-full">
    {list && (
      <div className={hasDetail ? 'hidden lg:flex flex-col' : 'min-h-screen w-full'}>
        {list}
      </div>
    )}
    <div className="p-4 grow flex">
      {children}
    </div>
  </div >
);
