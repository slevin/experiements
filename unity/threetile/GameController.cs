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
    private Vector3 location;
    private Tile tile;

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
}

public class Tile : Object
{
//    GameObject tilePrefab;
    GameObject tileObject;

    public Tile(GameObject tilePrefab, Vector3 location, Vector2 tileShift)
    {
        tileObject = Instantiate(tilePrefab, location, Quaternion.identity) as GameObject;
        tileObject.renderer.material.mainTextureOffset = tileShift;
    }

    public void instantiateGameObject()
    {
        
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
                    //Tile t = new Tile(controller.tilePrefab, location, controller.TileShift(pair));
                    GameObject tobj = controller.CreateTile(pair);
                }
            }
        }
    }

    /*
     * fill with tile objects 
     * scramble (forget that)
     * instantiate each one
     * position based on its tile location, so there is some sizer that knows how to draw
     * */
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
