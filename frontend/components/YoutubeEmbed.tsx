import { SimpleDialog } from 'components/Dialog';
import { PlayCircle } from 'react-feather';

export function YoutubeEmbed({
  title,
  thumbnail,
  children,
}: {
  title: string;
  thumbnail: string;
  children: React.ReactChild;
}) {
  return (
    <SimpleDialog
      title=""
      button={
        <div className="relative">
          <div className="absolute inset-2 flex justify-center">
            <div className="basis-1/3 text-red-700/80 text-shadow-lg">
              <PlayCircle className="w-full h-full" />
            </div>
          </div>
          <img className="h-full object-cover" alt={title} src={thumbnail} />
        </div>
      }
    >
      <div className="relative aspect-w-16 aspect-h-9">{children}</div>
    </SimpleDialog>
  );
}
