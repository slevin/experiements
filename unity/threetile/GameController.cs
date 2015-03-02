using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class IntPair
{
    public int x;
    public int y;

    public IntPair(int x, int y)
    {
        this.x = x;
        this.y = y;
    }

    public override bool Equals(object o)
    {
        return base.Equals (o);
    }

    public override int GetHashCode()
    {
        return x * y + x;
    }

    public IntPair Left()
    {
        return new IntPair (x - 1, y);
    }

    public IntPair Right()
    {
        return new IntPair (x + 1, y);
    }

    public IntPair Up()
    {
        return new IntPair (x, y - 1);
    }

    public IntPair Down()
    {
        return new IntPair (x, y + 1);
    }
}

public class IntPairComparer : IEqualityComparer<IntPair>
{
    public bool Equals(IntPair one, IntPair two)
    {
        return one.x == two.x && one.y == two.y;
    }

    public int GetHashCode(IntPair pair)
    {
        return pair.x * pair.y + pair.x;
    }
}

public class Slot : Object
{
    private IntPair pos;
    private Dictionary<IntPair, Slot> allSlots;
    public Vector3 location;
    private Tile _tile;
    public Tile tile
    {
        get {
            return this._tile;
        }
        set {
            this._tile = value;
            value.slot = this;
        }
    }

    public Slot(IntPair pos, Dictionary<IntPair, Slot> allSlots, Vector3 location)
    {
        this.pos = pos;
        this.allSlots = allSlots;
        this.location = location;
    }

    private Slot otherSlot(IntPair pos)
    {
        return allSlots [pos];
    }

    public Slot Left()
    {
        return otherSlot (pos.Left ());
    }

    public Slot Right()
    {
        return otherSlot (pos.Right ());
    }

    public Slot Up()
    {
        return otherSlot (pos.Up ());
    }

    public Slot Down()
    {
        return otherSlot (pos.Down ());
    }

    public Slot EmptyNeighbor() {
        Slot left = Left();
        if (left.tile == null) { return left; }
        Slot right = Right();
        if (right.tile == null) { return right; }
        Slot up = Up();
        if (up.tile == null) { return up; }
        Slot down = Down();
        if (down.tile == null) { return down; }
        return null;
    }
}

public class TileMove {
    public Slot fromSlot;
    public Slot toSlot;
    public float distanceOverTime;

    public TileMove(Slot fromSlot, Slot toSlot) {
        this.fromSlot = fromSlot;
        this.toSlot = toSlot;

        this.distanceOverTime = (fromSlot.location - toSlot.location).magnitude / 1.0f;
    }

    public Vector3 TargetPosition() {
        return this.toSlot.location;
    }

    public void CompleteMove() {
        toSlot.tile = fromSlot.tile;
        fromSlot.tile.tileMove = null;
        fromSlot.tile = null;
    }
}

public class Tile : Object
{

    private GameObject tileObject;
    public Slot slot;
    public TileMove tileMove;
    public Tile(GameObject tilePrefab, Vector3 location, Vector2 tileShift)
    {
        tileObject = Instantiate(tilePrefab, location, Quaternion.identity) as GameObject;
        tileObject.renderer.material.mainTextureOffset = tileShift;

        TileScript script = tileObject.GetComponent<TileScript>();
        script.tile = this;
    }

    public void OnClick() {
        Slot emptyNeighbor = this.slot.EmptyNeighbor();
        if (emptyNeighbor) {
            this.tileMove = new TileMove(this.slot, emptyNeighbor);
        }
    }
}

public class Board : Object
{
//    private int columns;
//    private int rows;

    public Board(int columns, int rows, GameController controller)
    {
//        this.columns = columns;
//        this.rows = rows;

        int count = rows * columns;
        Dictionary<IntPair, Slot> allSlots = new Dictionary<IntPair, Slot> (count, new IntPairComparer());
        for (int y = 0; y < rows; y++) {
            for (int x = 0; x < columns; x++) {
                IntPair pair = new IntPair (x, y);
                Vector3 location = controller.SlotLocation(pair);
                Slot slot = new Slot (pair, allSlots, location);
                allSlots.Add (pair, slot);

                bool lastOne = x == columns - 1 && y == rows - 1;
                if (!lastOne) {
                    Tile t = new Tile(controller.tilePrefab, location, controller.TileShift(pair));
                    slot.tile = t;
                }
            }
        }
    }

}

public class GameController : MonoBehaviour
{
    public GameObject tilePrefab;
    public float tileSpacing;
    public int rows;
    public int columns;
    public float tileSize;

    // Use this for initialization
    void Start()
    {
         Board board = new Board(columns, rows, this);

    }

    // Update is called once per frame
    void Update()
    {

    }

    public Vector3 SlotLocation(IntPair pos)
    {
        float totalWidth = this.columns * this.tileSize + (this.columns - 1) * this.tileSpacing;
        float totalHeight = this.rows * this.tileSize + (this.rows - 1) * this.tileSpacing;
        float halfWidth = totalWidth / 2.0f;
        float halfHeight = totalHeight / 2.0f;
        float myX = (this.tileSize + this.tileSpacing) * pos.x;
        float myY = (this.tileSize + this.tileSpacing) * pos.y;

        float p6nOffX = myX - halfWidth + (this.tileSize / 2.0f);
        float p6nOffY = totalHeight - myY - halfHeight + (this.tileSize / 2.0f);
        return new Vector3(p6nOffX, p6nOffY);
    }

    public Vector2 TileShift(IntPair pos) {
        float xTileShift = pos.x / (float)this.columns;
        float yTileShift = (this.rows - (pos.y + 1)) / (float)this.rows;
        return new Vector2(xTileShift, yTileShift);
    }

    public GameObject CreateTile(IntPair pos)
    {
        GameObject obj = Instantiate(this.tilePrefab, SlotLocation(pos), Quaternion.identity) as GameObject;
        obj.renderer.material.mainTextureOffset = TileShift(pos);
        return obj;
    }
}
