import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { PlayCircle } from 'lucide-react';

export function YoutubeEmbed({
  title,
  thumbnail,
  children,
}: {
  title: string;
  thumbnail: string;
  children: React.ReactNode;
}) {
  return (
    <Dialog>
      <DialogTrigger asChild>
        <div className="relative">
          <div className="absolute inset-2 flex justify-center">
            <div className="basis-1/3 text-red-700/80">
              <PlayCircle className="w-full h-full" />
            </div>
          </div>
          <img className="h-full object-cover" alt={title} src={thumbnail} />
        </div>
      </DialogTrigger>
      <DialogContent>
        <div className="relative aspect-w-16 aspect-h-9">{children}</div>
      </DialogContent>
    </Dialog>
  );
}
