import sharp from 'sharp';

export async function imageThumbnail(input: Uint8Array, width: number) {
  const image = sharp(input);
  const metadata = await image.metadata();
  if (metadata.format === 'svg' || (metadata.pages ?? 1) > 1) return;

  if (metadata.format === 'png') {
    const png = Buffer.from(input.buffer, input.byteOffset, input.byteLength);
    for (
      let offset = 8;
      offset + 12 <= png.length;
      offset += 12 + png.readUInt32BE(offset)
    ) {
      const type = png.toString('ascii', offset + 4, offset + 8);
      if (type === 'acTL') return;
      if (type === 'IDAT') break;
    }
  }

  return image
    .autoOrient()
    .resize({ width, withoutEnlargement: true })
    .webp({ quality: 75 })
    .toBuffer();
}
