// Render Rex's program glyphs (block-glyph-* in SuperBlockIcons.bundle) to PNG icons
// for Alfred. Monochrome glyphs are drawn light on a dark rounded tile so they read on
// light and dark Alfred themes alike; colored glyphs keep their colors on the same tile.
//
// The workflow ships this compiled and runs it against the installed Rex app, so the
// release carries none of Rex's artwork.
//
//   extract_rex_icons REX_APP OUTPUT_DIR
import AppKit

let arguments = CommandLine.arguments
guard arguments.count == 3 else {
    FileHandle.standardError.write("usage: extract_rex_icons REX_APP OUTPUT_DIR\n".data(using: .utf8)!)
    exit(2)
}
let bundlePath = arguments[1] + "/Contents/Resources/SuperBlockIcons_SuperBlockIcons.bundle"
let outputDirectory = URL(fileURLWithPath: arguments[2])
guard let bundle = Bundle(path: bundlePath) else {
    FileHandle.standardError.write("no icon bundle at \(bundlePath)\n".data(using: .utf8)!)
    exit(1)
}

let assetCatalog = bundlePath + "/Contents/Resources/Assets.car"
let glyphPrefix = "block-glyph-"

// Asset names sit in Assets.car as plain ASCII; scan for the glyph prefix rather than
// shelling out to assetutil, which needs Xcode. Names that aren't real assets are
// dropped later when the bundle can't load them.
func glyphNamesInCatalog() -> [String] {
    guard let data = FileManager.default.contents(atPath: assetCatalog) else { return [] }
    let bytes = [UInt8](data)
    let prefix = [UInt8](glyphPrefix.utf8)
    func isNameByte(_ byte: UInt8) -> Bool {
        (byte >= 0x61 && byte <= 0x7a) || (byte >= 0x30 && byte <= 0x39) || byte == 0x2d
            || byte == 0x2e || byte == 0x5f || byte == 0x2b
    }
    var names = Set<String>()
    var index = 0
    while index + prefix.count <= bytes.count {
        if bytes[index] == prefix[0] && Array(bytes[index..<index + prefix.count]) == prefix {
            var end = index + prefix.count
            while end < bytes.count && isNameByte(bytes[end]) { end += 1 }
            if end > index + prefix.count, let name = String(bytes: bytes[index..<end], encoding: .ascii) {
                names.insert(name)
            }
            index = end
        } else {
            index += 1
        }
    }
    return names.sorted()
}

let pixelSize = 128
let tileColor = NSColor(srgbRed: 0.17, green: 0.18, blue: 0.21, alpha: 1)
let glyphColor = NSColor(srgbRed: 0.90, green: 0.91, blue: 0.93, alpha: 1)

func bitmapOfSize(_ size: Int) -> NSBitmapImageRep {
    NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: size, pixelsHigh: size,
                     bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                     colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
}

func draw(into bitmap: NSBitmapImageRep, _ body: () -> Void) {
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)
    body()
    NSGraphicsContext.restoreGraphicsState()
}

// A glyph is "monochrome" if none of its visible pixels carry color.
func isMonochrome(_ image: NSImage) -> Bool {
    let sample = bitmapOfSize(64)
    draw(into: sample) { image.draw(in: NSRect(x: 0, y: 0, width: 64, height: 64)) }
    for x in 0..<64 {
        for y in 0..<64 {
            guard let color = sample.colorAt(x: x, y: y), color.alphaComponent > 0.1 else { continue }
            let spread = max(color.redComponent, color.greenComponent, color.blueComponent)
                - min(color.redComponent, color.greenComponent, color.blueComponent)
            if spread > 0.08 { return false }
        }
    }
    return true
}

// Fill the glyph's shape with one color, using the glyph as a mask.
func tinted(_ image: NSImage, with color: NSColor) -> NSImage {
    let result = NSImage(size: image.size)
    result.lockFocus()
    image.draw(in: NSRect(origin: .zero, size: image.size))
    color.set()
    NSRect(origin: .zero, size: image.size).fill(using: .sourceAtop)
    result.unlockFocus()
    return result
}

func aspectFitRect(for imageSize: NSSize, in bounds: NSRect) -> NSRect {
    let scale = min(bounds.width / imageSize.width, bounds.height / imageSize.height)
    let size = NSSize(width: imageSize.width * scale, height: imageSize.height * scale)
    return NSRect(x: bounds.midX - size.width / 2, y: bounds.midY - size.height / 2,
                  width: size.width, height: size.height)
}

func iconPNG(for glyph: NSImage) -> Data {
    let glyphToDraw = isMonochrome(glyph) ? tinted(glyph, with: glyphColor) : glyph
    let bitmap = bitmapOfSize(pixelSize)
    let full = NSRect(x: 0, y: 0, width: pixelSize, height: pixelSize)
    draw(into: bitmap) {
        let tile = full.insetBy(dx: 6, dy: 6)
        tileColor.set()
        NSBezierPath(roundedRect: tile, xRadius: 26, yRadius: 26).fill()
        glyphToDraw.draw(in: aspectFitRect(for: glyphToDraw.size, in: tile.insetBy(dx: 22, dy: 22)))
    }
    return bitmap.representation(using: .png, properties: [:])!
}

try FileManager.default.createDirectory(at: outputDirectory, withIntermediateDirectories: true)
var written = 0
for name in glyphNamesInCatalog() {
    guard let glyph = bundle.image(forResource: name) else { continue }
    let programName = String(name.dropFirst(glyphPrefix.count))
    try iconPNG(for: glyph).write(to: outputDirectory.appendingPathComponent(programName + ".png"))
    written += 1
}
print("wrote \(written) icons to \(outputDirectory.path)")
